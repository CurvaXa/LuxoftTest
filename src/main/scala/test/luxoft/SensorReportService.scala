package test.luxoft

import java.io.File

import scala.collection.immutable.ListMap
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.stream.alpakka.csv.scaladsl.{CsvParsing, CsvToMap}
import akka.stream.scaladsl.FileIO
import akka.util.Timeout
import test.luxoft.Helpers.{combineAvg, getListOfFiles, _}
import test.luxoft.SensorReportActor.GetReport
import test.luxoft.SensorReportService.mergeStats
import test.luxoft.models.{FullSensorReport, SensorData, SensorReport, SensorStats}


class SensorReportService(implicit val actorSystem: ActorSystem, implicit val ec: ExecutionContext) {

  def generateReport(dirName: String): Future[FullSensorReport] = {
    val reportsFutureList = getListOfFiles(dirName)
      .filter(_.getName.split("\\.").lastOption.contains("csv"))
      .map(f => processFile(f).map(r => (f, r)))

    Future.sequence(reportsFutureList).map { reportMapList =>
      val (successfulReports, failedReports) = reportMapList.span { case (_, result) => result.isSuccess }

      handleFailedFiles(failedReports.collect { case (f, Failure(ex)) => (f, ex) })

      val reportMap = successfulReports.collect { case (_, Success(s)) => s }
        .foldLeft(Map.empty[String, SensorReport]) {
          (base, item) => base.merge(item)(mergeStats)
        }

      val failedCount = reportMap.values.foldLeft(0)((a, b) => a + b.failedCount)
      val sortedMap = reportMap.toSeq.sortWith { case ((_, s1), (_, s2)) =>
        s1.stats.map(_.avg).getOrElse(.0) > s2.stats.map(_.avg).getOrElse(.0)
      }.to(ListMap)

      models.FullSensorReport(sortedMap,
        failedCount = failedCount,
        totalCount = reportMap.values.foldLeft(0)((a, b) => a + b.stats.map(_.count).getOrElse(0)) + failedCount,
        fileCount = successfulReports.size)
    }
  }

  private def handleFailedFiles(failedReports: List[(File, Throwable)]): Unit = {
    // Well, we need a file name here, but let's leave it out of the task scope
    failedReports.foreach { case (file, ex) =>
      println(s"File ${file.getName} failed with exception $ex")
    }
  }

  def processFile(file: File): Future[Try[Map[String, SensorReport]]] = {
    val ACTOR_PARALLELISM = 3 // Message queue size
    implicit val askTimeout: Timeout = 5.seconds
    val myActor = actorSystem.actorOf(Props[SensorReportActor])

    def streamFuture = FileIO.fromPath(file.toPath)
      .via(CsvParsing.lineScanner())
      .via(CsvToMap.toMap())
      .map { row =>
        (row.get("sensor-id").map(_.utf8String),
          row.get("humidity").flatMap(_.utf8String.toIntOption)
        )
      }
      .collect { case (Some(id), value) =>
        SensorData(id, value)
      }
      .ask[Unit](ACTOR_PARALLELISM)(myActor)
      .run()

    def reportFuture = for {
      _ <- streamFuture
      report <- myActor.ask(GetReport).mapTo[Map[String, SensorReport]]
    } yield {
      Success(report)
    }

    reportFuture.recover {
      case NonFatal(ex) => Failure(ex)
    }
  }

}

object SensorReportService {

  def mergeStats(baseReportOpt: Option[SensorReport], addReport: SensorReport): SensorReport = {
    baseReportOpt.map { baseReport =>
      val newStats = (baseReport.stats, addReport.stats) match {
        case (Some(baseStats), Some(addStats)) =>
          val newAvg = combineAvg(baseStats.avg, baseStats.count, addStats.avg, addStats.count)
          Some(SensorStats(
            min = Math.min(baseStats.min, addStats.min),
            avg = newAvg,
            max = Math.max(baseStats.max, addStats.max),
            count = baseStats.count + addStats.count))
        case (None, Some(addStat)) => Some(addStat)
        case (None, None) => None
      }
      SensorReport(newStats, baseReport.failedCount + addReport.failedCount)
    }.getOrElse(addReport)
  }
}


package test.luxoft

import scala.collection.mutable

import akka.actor.Actor
import test.luxoft.Helpers.combineAvg
import test.luxoft.SensorReportActor.GetReport
import test.luxoft.models.{SensorData, SensorReport, SensorStats}

object SensorReportActor {
  case object GetReport
}

class SensorReportActor extends Actor {

  val sensorStatsMap = mutable.Map.empty[String, SensorReport]

  def receive: Receive = {
    case SensorData(id, value) =>
      val newReport = (sensorStatsMap.get(id), value) match {
        case (Some(SensorReport(Some(SensorStats(min, avg, max, count)), failedCount)), Some(newVal)) =>
          val newStats = SensorStats(Math.min(newVal, min),
            combineAvg(avg, count, newVal, 1),
            Math.max(newVal, max),
            count + 1)
          SensorReport(Some(newStats), failedCount)
        case (Some(report), None) =>
          report.copy(failedCount = report.failedCount + 1)
        case (None, Some(v)) =>
          SensorReport(Some(SensorStats(v, v, v, 1)), failedCount = 0)
        case (None, None) =>
          SensorReport(None, failedCount = 1)

      }
      sensorStatsMap.update(id, newReport)
      sender() ! ()

    case GetReport =>
      val result: Map[String, SensorReport] = sensorStatsMap.map(kv => (kv._1, kv._2)).toMap
      sender() ! result

  }
}
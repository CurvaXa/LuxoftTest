import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

import test.luxoft.SensorReportService

object Main {
  implicit val actorSystem: ActorSystem = ActorSystem("actorSystem")

  def main(args: Array[String]): Unit = {
    val reportService = new SensorReportService()
    reportService.generateReport("reports")
      .andThen {
        case Success(report) =>
          println(report.toFormattedSting)
        case Failure(ex) =>
          println(s"Oops, $ex")
      }.andThen { _ =>
      actorSystem.terminate()
    }
  }
}
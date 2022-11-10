package test.luxoft

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import test.luxoft.models.{SensorReport, SensorStats}

class SensorReportServiceSpec extends AnyFlatSpec {

  val reportFailed = SensorReport(None, 1)
  val report1 = SensorReport(Some(SensorStats(1, 1.0, 1, 1)), 1)
  val report2 = SensorReport(Some(SensorStats(3, 3, 3, 1)), 1)

  "SensorReportService.mergeStats" should "merge stats with failed case" in {
    SensorReportService.mergeStats(Some(reportFailed), reportFailed) shouldBe
      SensorReport(None, 2)
  }

  "SensorReportService.mergeStats" should "merge stats with avg case" in {
    SensorReportService.mergeStats(Some(report1), report2) shouldBe
      SensorReport(Some(SensorStats(1, 2.0, 3, 2)), 2)
  }
}
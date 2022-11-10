package test.luxoft.models

import scala.collection.immutable.ListMap
import scala.collection.mutable

case class FullSensorReport(sensors: ListMap[String, SensorReport], failedCount: Int, totalCount: Int, fileCount: Int) {
  def toFormattedSting: String = {
    val sb = new mutable.StringBuilder("")

    sb ++=
      s"""
         |Num of processed files: $fileCount
         |Num of processed measurements: $totalCount
         |Num of failed measurements: $failedCount
         |
         |Sensors with highest avg humidity:
         |
         |sensor-id,min,avg,max\n""".stripMargin

    // More effective than pure functional solution
    sensors.foldLeft(sb) { case (sb, (id, report)) =>
      sb ++= (s"$id,${report.stats.map(_.min).getOrElse("NaN")},${report.stats.map(_.avg.toInt).getOrElse("NaN")}," +
        s"${report.stats.map(_.max).getOrElse("NaN")}\n")
      sb
    }
    sb.toString()
  }
}

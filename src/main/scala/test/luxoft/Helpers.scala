package test.luxoft

import java.io.File

object Helpers {
  implicit class MapExtended[K, V1](val map: Map[K, V1]) extends AnyVal {
    def merge[V2](mapB: Map[K, V2])(f: (Option[V1], V2) => V1): Map[K, V1] =
      mapB.foldLeft(map) { case (baseMap, (k, v2)) =>
        baseMap + (k -> f(baseMap.get(k: K), v2))
      }
  }

  def combineAvg(avg1: Double, weight1: Int, avg2: Double, weight2: Int): Double =
    (avg1 * weight1 + avg2 * weight2) / (weight1 + weight2)

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
}

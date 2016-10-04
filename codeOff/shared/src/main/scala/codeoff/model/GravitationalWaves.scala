package codeoff.model

import scala.collection.immutable.HashMap

object GravitationalWaves {

  case class IntensityPlane(width: Int, height: Int, rep: Map[Location, Int]) {

    lazy val minX: Int = rep.keySet.map(_.x).min
    lazy val maxX: Int = rep.keySet.map(_.x).max
    lazy val minY: Int = rep.keySet.map(_.y).min
    lazy val maxY: Int = rep.keySet.map(_.y).max

    lazy val middle: Location = Location((width + 1)/2,(height + 1)/2)

    lazy val layerLocations: Map[Int, Set[Location]] = {
      def go(layer: Int, xPlane:(Int, Int), yPlane: (Int ,Int), accum: Map[Int, Set[Location]]): Map[Int, Set[Location]] = {
        val nextX = (xPlane._1 - 1, xPlane._2 + 1)
        val nextY = (yPlane._1 - 1, yPlane._2 + 1)

        if (rep.get(Location(nextX._1, nextY._1)).isDefined) {
          val band: Set[Location] = Location.band(nextX._1, nextX._2, nextY._1, nextY._2).toSet
          val nextLayer = layer + 1
          go(nextLayer, nextX, nextY, accum + ((nextLayer, band)))
        }
        else
          accum
      }

      go(1, (middle.x, middle.x),(middle.y, middle.y), HashMap(1 -> Set(middle)))
    }

    /*lazy val layers: Map[Int, Map[Location, Int]] =

    lazy val outer = layers.get(1)  */

    lazy val draw: List[String] = Location.orderedStream(width, height).toList.map(x =>
      rep.get(x).fold("0")(_.toString)).grouped(width).map(_.mkString(" ")).toList
  }
}

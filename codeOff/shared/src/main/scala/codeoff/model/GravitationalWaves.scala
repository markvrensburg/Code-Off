package codeoff.model

import scala.collection.immutable.HashMap
import scalaz._
import Scalaz._
import scala.annotation.tailrec

object GravitationalWaves {

  case class IntensityPlane(width: Int, height: Int, rep: Map[Location, Int]) {

    lazy val minX: Int = rep.keySet.map(_.x).min
    lazy val maxX: Int = rep.keySet.map(_.x).max
    lazy val minY: Int = rep.keySet.map(_.y).min
    lazy val maxY: Int = rep.keySet.map(_.y).max

    lazy val middle: Location = Location(width/2,height/2)

    lazy val layerLocations: Map[Int, Set[Location]] = {
      @tailrec
      def go(layer: Int, topLeft: Location, bottomRight: Location, accum: Map[Int, Set[Location]]): Map[Int, Set[Location]] = {
        if (rep.get(topLeft).isDefined && rep.get(bottomRight).isDefined) {
          val band = Location.band(topLeft, bottomRight).toSet
          go(layer + 1, Location(topLeft.x-1, topLeft.y-1), Location(bottomRight.x+1, bottomRight.y+1), accum + ((layer, rep.keySet.intersect(band))))
        }
        else
          accum
      }
      go(1, middle, middle, HashMap.empty)
    }

    def layerOf(location: Location): Option[Int] = layerLocations.find(_._2.contains(location)).map(_._1)

    def neighbours(location: Location): Set[Location] = layerOf(location).flatMap(layer => {
      layerLocations.get(layer + 1).map(_.intersect(Direction.allDirections.map(location(_))))
    }).getOrElse(Set.empty)

    def intensity(location: Location): Int = neighbours(location).toList.traverse(rep.get).map(x =>
      Math.round((x.sum: Double)/x.size).toInt-1).getOrElse(0)

    def calculateIntensities(layer: Int): IntensityPlane = layerLocations.get(layer).fold(this)(l => {
      if (l.isEmpty)
        this
      else
        IntensityPlane(width,height, rep ++ l.map(x => (x, intensity(x))))
    })

    lazy val calculateIntensities: IntensityPlane = {
      def go(layer: Int, ip: IntensityPlane): IntensityPlane = {
        if (layer == 0)
          ip
        else
          go(layer-1, ip.calculateIntensities(layer))
      }
      go(layerLocations.keySet.toList.sorted.reverse.drop(1).head, this)
    }

    lazy val draw: List[String] = Location.orderedStream(width, height).toList.map(x =>
      rep.get(x).fold("0")(_.toString)).grouped(width).map(_.mkString(" ")).toList
  }
}

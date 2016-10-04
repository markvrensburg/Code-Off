package codeoff.parser

import codeoff.model.Location
import codeoff.model.GravitationalWaves._
import fastparse.all._

import scala.util.Try

object IntensityPlaneParser {

  private[this] val number: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))

  val intensityPlane = P((number ~ " ".?).rep.map(i => {
    val size = math.sqrt(i.size).toInt
    val locs = Location.orderedStream(size,size).zip(i)
    IntensityPlane(size, size, locs.toMap)
  }))

  def parse(input: String): Try[IntensityPlane] = Try(intensityPlane.parse(input).get.value)

}

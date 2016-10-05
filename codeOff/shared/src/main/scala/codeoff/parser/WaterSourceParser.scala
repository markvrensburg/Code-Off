package codeoff.parser

import codeoff.model.WaterSource._
import codeoff.model.Location
import fastparse.all._

import scala.util.Try

object WaterSourceParser {

  private[this] val entity = P(CharIn("#.*").!.map(x => Entity.read(x.head)))

  val waterSource = P(entity.rep.map(es => {
    val size = math.sqrt(es.size).toInt
    val locs = Location.orderedStream(size,size).zip(es)
    WaterSource(size, size, locs.toMap)
  }))

  def parse(input: String): Try[WaterSource] = Try(waterSource.parse(input).get.value)
}

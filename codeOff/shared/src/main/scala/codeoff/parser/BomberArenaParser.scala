package codeoff.parser

import codeoff.model.Bomber._
import codeoff.model.Location
import fastparse.all._

import scala.util.Try

object BomberArenaParser {

  private[this] val entity = P((CharIn("#*") | CharIn('0'to'9')).!.map(x => Entity.read(x.head)))

  val bomberArena = P(entity.rep.map(es => {
    val size = math.sqrt(es.size).toInt
    val locs = Location.orderedStream(size,size).zip(es)
    BomberArena(size, size, locs.toMap)
  }))

  def parse(input: String): Try[BomberArena] = Try(bomberArena.parse(input).get.value)
}

package codeoff.parser

import codeoff.model.Location
import codeoff.model.Maze._
import fastparse.all._

import scala.util.Try

object MazeParser {

  private[this] val entity = P((CharIn(" #@U.") | CharIn('0'to'9')).!.map(x => Entity.read(x.head)))

  val maze = P(entity.rep.map(es => {
    val size = math.sqrt(es.size).toInt
    val locs = Location.orderedStream(size,size).zip(es)
    Maze(size, size, locs.toMap)
  }))

  def parse(input: String): Try[Maze] = Try(maze.parse(input).get.value)
}

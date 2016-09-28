package codeoff.core.model

import scala.annotation.tailrec

final case class Location(x: Int, y: Int) {

  def apply(direction: Direction): Location = direction match {
    case Up => Location(x, y - 1)
    case Down => Location(x, y + 1)
    case Left => Location(x - 1, y)
    case Right => Location(x + 1, y)
  }

  def apply(direction: Direction, steps: Int): Location = {
    @tailrec
    def go(current: Location, count: Int): Location = {
      if (count <= 0)
        current
      else
        go(current(direction), count - 1)
    }
    go(this, steps)
  }
}

object Location {

  def apply(pair: (Int, Int)): Location = Location(pair._1, pair._2)

  def orderedStream(width: Int, height: Int, xOffset: Int = 0, yOffset: Int = 0): Stream[Location] = {
    Stream.iterate(Location(xOffset, yOffset), width * height)(l => {
      if (l.x == (width - 1))
        Location(0, l.y + 1)
      else
        Location(l.x + 1, l.y)
    })
  }

  implicit val locationOrdering: Ordering[Location] = new Ordering[Location] {
    override def compare(l1: Location, l2: Location): Int = {
      if (l1.y > l2.y) 1
      else if (l1.y == l2.y) {
        if (l1.x > l2.x) 1
        else if (l1.x == l2.x) 0
        else -1
      }
      else -1
    }
  }
}

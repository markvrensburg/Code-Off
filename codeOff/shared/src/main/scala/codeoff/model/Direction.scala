package codeoff.model

sealed trait Direction extends Product with Serializable {

  def fold[A](n: => A,
              ne: => A,
              e: => A,
              se: => A,
              s: => A,
              sw: => A,
              w: => A,
              nw: => A): A =
    this match {
      case North => n
      case NorthEast => ne
      case East => e
      case SouthEast => se
      case South => s
      case SouthWest => sw
      case West => w
      case NorthWest => nw
    }
}


case object North extends Direction

case object West extends Direction

case object East extends Direction

case object South extends Direction

case object NorthEast extends Direction

case object SouthEast extends Direction

case object SouthWest extends Direction

case object NorthWest extends Direction


object Direction {

  val simpleDirections: Set[Direction] = Set(
    North,
    East,
    South,
    West
  )

  val allDirections: Set[Direction] = Set(
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest
  )

  val north: Direction = North

  val west: Direction = West

  val east: Direction = East

  val south: Direction = South

  def opposite(direction: Direction): Direction =
    direction.fold(South, SouthWest, West, NorthWest, North, NorthEast, East, SouthEast)
}

package codeoff.core.model

sealed trait Direction extends Product with Serializable {

  def fold[A](up: => A,
              left: => A,
              right: => A,
              down: => A): A =
    this match {
      case Up => up
      case Left => left
      case Right => right
      case Down => down
    }
}

case object Up extends Direction

case object Left extends Direction

case object Right extends Direction

case object Down extends Direction

object Direction {

  val directions: Set[Direction] = Set(
    Up,
    Left,
    Right,
    Down
  )

  val up: Direction = Up

  val left: Direction = Left

  val right: Direction = Right

  val down: Direction = Down

  def opposite(direction: Direction): Direction =
    direction.fold(Down, Right, Left, Up)
}

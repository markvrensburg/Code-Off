package codeoff.core.model

object Maze {

  sealed trait Entity extends Product with Serializable {
    def fold[A](empty: => A, wall: => A, subject: => A, goal: => A, marked: => A): A = this match {
      case Empty => empty
      case Wall => wall
      case Subject => subject
      case Goal => goal
      case Marked => marked
    }
  }
  case object Empty extends Entity
  case object Wall extends Entity
  case object Subject extends Entity
  case object Goal extends Entity
  case object Marked extends Entity

  object Entity {

    def show(entity: Entity): Char = entity.fold(' ', '#', '@', 'U', '.')

    def read(char: Char): Entity =  char match {
      case ' ' => Empty
      case'#' => Wall
      case'@' => Subject
      case'U' => Goal
      case'.' => Goal
      case _ => Wall
    }
  }

  case class Maze(width: Int, height: Int, rep: Map[Location, Entity]) {

    lazy val current: Option[Location] = rep.find(_._2 == Subject).map(_._1)
    lazy val goals: Set[Location] = rep.filter(_._2 == Goal).keySet

    def apply(direction: Direction): Maze = current.fold(this)(curr => apply(curr(direction)))

    def apply(location: Location): Maze = current.fold(this){curr =>
      if (neighbours(curr).contains(location) && canMoveTo(location)) {
        Maze(width, height, rep + ((curr, Empty)) + ((location, Subject)))
      }
      else this
    }

    def canMoveTo(location: Location): Boolean =
      rep.get(location).fold(false)(_.fold(true,false,true,true,true))

    def neighbours(location: Location): Set[Location] =
      Direction.directions.map(location(_)).filter(rep.get(_).isDefined)

    def mark(locations: Set[Location]): Maze = Maze(width, height, rep ++ locations.map((_, Marked)))

    lazy val actions: Set[Location] = current.fold(Set.empty[Location])(curr => neighbours(curr).filter(canMoveTo))

    lazy val draw: List[String] = Location.orderedStream(width, height).toList.map(x =>
      rep.get(x).fold(Entity.show(Wall))(Entity.show)).grouped(width).map(_.mkString).toList
  }
}

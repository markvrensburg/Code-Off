package codeoff.model

import quiver._

object WaterSource {

  sealed trait Entity extends Product with Serializable {
    import Entity._
    def fold[A](wall: => A, water: => A, marked: => A): A = this match {
      case Wall => wall
      case Water => water
      case Marked => marked
    }
  }

  object Entity {

    case object Wall extends Entity

    case object Water extends Entity

    case object Marked extends Entity

    def show(entity: Entity): Char =
      entity.fold('#', '.', '*')

    def read(char: Char): Entity =  char match {
      case '#' => Wall
      case '.' => Water
      case '*' => Marked
      case _ => Wall
    }
  }

  case class WaterSource(width: Int, height: Int, rep: Map[Location, Entity]) {
    import Entity._

    lazy val allWater: Set[Location] = rep.filter(_._2 == Water).keySet

    def waterBody(location: Location): Set[Location] = graph.labfilter(_ == Water).bfs(location).toSet

    def neighbours(location: Location): Set[Location] =
      Direction.simpleDirections.map(location(_)).filter(rep.get(_).isDefined)

    lazy val graph: Graph[Location, Entity, Unit] = rep.toVector.map(x => {
      val edges = neighbours(x._1).toVector.map(() -> _)
      Context(edges, x._1, x._2, edges)
    }).foldLeft(empty[Location, Entity, Unit])((x, y) => y.embed(x))

    lazy val waterRegions: Vector[Set[Location]] = {

      val water = graph.labfilter(_ == Water)

      def go(check: Vector[Location], accum: Vector[Set[Location]]): Vector[Set[Location]] = {
        check.headOption.fold(accum)(l => {
          val region = water.bfs(l).toVector
          go(check.diff(region), accum :+ region.toSet)
        })
      }

      go(water.nodes, Vector.empty)
    }

    def mark(locations: Set[Location]): WaterSource = WaterSource(width, height, rep ++ locations.map((_, Marked)))

    lazy val draw: List[String] = Location.orderedStream(width, height).toList.map(x =>
      rep.get(x).fold(Entity.show(Wall))(Entity.show)).grouped(width).map(_.mkString).toList
  }
}

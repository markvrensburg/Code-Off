package codeoff.model

import codeoff.search.{AStarMutable, InformedSearchNode, SearchTree, _}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

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

    def locationHeuristic(goals: Set[Location]): Heuristic[Location] = l => goals.map(g => Distance.manhattan(l, g)).min

    def locationGoal(goals: Set[Location]): Goal[Location] = l => goals.flatMap(neighbours).filter(isWall).contains(l)

    def transition(h: Heuristic[Location]): Transition[InformedSearchNode[Location, Location]] = node => {
      neighbours(node.state).filter(isWall).toStream.map(loc => {
        val nextState = loc
        InformedSearchNode(nextState, loc, node.g + 1, h(nextState))
      })
    }

    def shortestPath(from: Set[Location], to: Set[Location]): List[Location] = {

      val goal = locationGoal(to)
      val h = locationHeuristic(to)

      def go(location: Location): List[Location] = {
        val initial = InformedSearchNode(location, location, 0, h(location))
        val searchTree = SearchTree(initial, transition(h))
        AStarMutable.runPlan(searchTree, goal)
      }

      if (from.intersect(to).isEmpty)from.map(go).minBy(_.size) else List.empty
    }

    lazy val allWater: Set[Location] = rep.filter(_._2 == Water).keySet

    lazy val allWall: Set[Location] = rep.filter(_._2 == Wall).keySet

    def isWall(location: Location): Boolean = rep.get(location).fold(false)(_ == Wall)

    def isWater(location: Location): Boolean = rep.get(location).fold(false)(_ == Water)

    lazy val waterEdges: Set[Location] = allWater.filter(x => neighbours(x).map(rep.get).exists{
      case Some(Wall) => true
      case _ => false
    })

    def removeWall(locations: Set[Location]): WaterSource = {
      val walls = locations.intersect(rep.keySet).filter(x => rep.get(x) match {
        case Some(Wall) => true
        case _ => false
      })
      WaterSource(width, height, rep ++ walls.map(x => (x, Water)))
    }

    def neighbours(location: Location): Set[Location] =
      Direction.simpleDirections.map(location(_)).filter(rep.get(_).isDefined)

    private[this] def waterBfs(location: Location): Set[Location] = {
      @tailrec
      def go(frontier: Set[Location], seen: Set[Location]): Set[Location] = {
       if (frontier.isEmpty)
         seen
       else {
         val toCheck = frontier.flatMap(x => neighbours(x).filter(y => isWater(y) && !seen.contains(y)))
         go(toCheck, seen ++ frontier)
       }
      }
      go(Set(location), HashSet.empty)
    }

    lazy val waterRegions: Vector[Set[Location]] = {
      @tailrec
      def go(check: Set[Location], accum: Vector[Set[Location]]): Vector[Set[Location]] = check.headOption match {
        case Some(l) =>
          val region = waterBfs(l)
          go(check.diff(region), accum :+ region)
        case None => accum
      }
      go(allWater, Vector.empty)
    }

    def mark(locations: Set[Location]): WaterSource = WaterSource(width, height, rep ++ locations.map((_, Marked)))

    lazy val draw: List[String] = Location.orderedStream(width, height).toList.map(x =>
      rep.get(x).fold(Entity.show(Wall))(Entity.show)).grouped(width).map(_.mkString).toList
  }
}

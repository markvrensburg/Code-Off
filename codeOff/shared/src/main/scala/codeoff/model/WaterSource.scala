package codeoff.model

import codeoff.search.SearchTree

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scalaz.Tree

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

    lazy val allWall: Set[Location] = rep.filter(_._2 == Wall).keySet

    def isWall(location: Location): Boolean = rep.get(location).fold(false)(_ == Wall)

    def isWater(location: Location): Boolean = rep.get(location).fold(false)(_ == Water)

    lazy val waterEdges: Set[Location] = allWater.filter(x => neighbours(x).map(rep.get).exists{
      case Some(Wall) => true
      case _ => false
    })

    lazy val wallEdges: Set[Location] = allWall.filter(x => neighbours(x).map(rep.get).exists{
      case Some(Water) => true
      case _ => false
    })

    private[this] lazy val indexedRegions = waterRegions.zipWithIndex

    private[this] def regionIndex(location: Location): Option[Int] =
      indexedRegions.find(_._1.contains(location)).map(_._2)

    def borders(locations: Set[Location]): Set[Int] = neighbours(locations.filter(isWall)).flatMap(x => {
      regionIndex(x) match {
        case Some(r) => HashSet(r)
        case _ => HashSet.empty[Int]
      }
    })

    private[this] def edgePath(from: Location): Tree[(Location, Set[Location])] = {
      SearchTree[(Location, Set[Location])]((from, HashSet(from)), n => {
        val current = n._1
        val accum = n._2
        neighbours(current).filter(isWall).map(l => (l, accum + l)).toStream
      }).tree
    }

    private[this] val edgePaths: Tree[Set[Location]] =
      Tree.Node(HashSet.empty[Location], wallEdges.toStream.map(x => edgePath(x).map(_._2)))

    def wallEdgesDepth(depth: Int): Vector[Set[Location]] =
      edgePaths.levels(depth).toVector.filter(l => borders(l).size > 1)

    def removeWall(locations: Set[Location]): WaterSource = {
      val walls = locations.intersect(rep.keySet).filter(x => rep.get(x) match {
        case Some(Wall) => true
        case _ => false
      })
      WaterSource(width, height, rep ++ walls.map(x => (x, Water)))
    }

    def addWall(locations: Set[Location]): WaterSource = {
      val walls = locations.intersect(rep.keySet).filter(x => rep.get(x) match {
        case Some(Water) => true
        case _ => false
      })
      WaterSource(width, height, rep ++ walls.map(x => (x, Wall)))
    }

    def neighbours(location: Location): Set[Location] =
      Direction.simpleDirections.map(location(_)).filter(rep.get(_).isDefined)

    def neighbours(locations: Set[Location]): Set[Location] = locations.flatMap(neighbours)

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
      go(HashSet(location), HashSet.empty)
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

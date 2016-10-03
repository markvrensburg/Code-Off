package codeoff.model

import scala.annotation.tailrec

object Bomber {

  sealed trait Entity extends Product with Serializable {
    import Entity._
    def fold[A](wall: => A,
                bomb: Int => A,
                explosion: => A): A = this match {
      case Wall => wall
      case Bomb(n) => bomb(n)
      case Explosion => explosion
    }
  }

  object Entity {

    case object Wall extends Entity

    case class Bomb(radius: Int) extends Entity

    case object Explosion extends Entity

    def show(entity: Entity): Char =
      entity.fold('#', _.toString.head, '*')

    def read(char: Char): Entity =  char match {
      case '#' => Wall
      case d if d.isDigit => Bomb(d.toString.toInt)
      case '*' => Explosion
      case _ => Wall
    }
  }

  case class BomberArena(width: Int, height: Int, rep: Map[Location, Entity]) {

    import Entity._

    def apply(bomb: Block[Bomb]): BomberArena = {
      if (validLocation(bomb.location))
        BomberArena(width,height, rep + bomb.tupled)
      else
        this
    }

    def apply(bombs: Set[Block[Bomb]]): BomberArena = bombs.foldLeft(this)((arena, bomb) => arena(bomb))

    def validLocation(location: Location): Boolean = rep.get(location).isDefined

    def adjacentBonus(f: (Int, Int) => Int): BomberArena = {
      val updated = rep.map(x => x._2 match {
        case Bomb(r) => (x._1, Bomb(f(r,neighbourBombs(x._1))))
        case _ => (x._1, x._2)
      })
      BomberArena(width,height,updated)
    }

    def explodedRegion(location: Location, radius: Int): Vector[Location] = {
      @tailrec
      def go(location: Location, steps: Int, accum: Vector[Location]): Vector[Location] = {
        if (steps == 0)
          Vector(location) ++ accum
        else {
          go(location, steps - 1, accum ++ Direction.simpleDirections.map(location(_, steps)))
        }
      }
      go(location, radius, Vector.empty)
    }

    def explosionAt(location: Location): Boolean = rep.get(location).fold(true) {
      case Explosion => true
      case _ => false
    }

    def bombAt(location: Location): Option[Block[Bomb]] = rep.get(location).flatMap {
      case b: Bomb => Some(Block(location, b))
      case _ => None
    }

    lazy val explosions: Set[Location] = rep.keySet.filter(explosionAt)

    lazy val bombs: Vector[Block[Bomb]] = rep.keySet.toVector.flatMap(bombAt(_) match {
      case None => Vector.empty
      case Some(b) => Vector(b)
    })

    lazy val explosionRegion: Set[Location] = bombs.flatMap(x =>
      explodedRegion(x.location, x.entity.radius)).toSet.intersect(rep.keySet)

    lazy val explodeBombs: BomberArena = BomberArena(width,height, rep ++ explosionRegion.map((_, Explosion)))

    def neighbourBombs(location: Location): Int = Direction.allDirections.map(location(_)).toList.map(rep.get(_).fold(0){
        case Bomb(_) => 1
        case _ => 0
      }).sum

    lazy val draw: List[String] = Location.orderedStream(width, height).toList.map(x =>
      rep.get(x).fold(Entity.show(Wall))(Entity.show)).grouped(width).map(_.mkString).toList
  }

  object BomberArena {

    import Entity._

    def walls(width: Int, height: Int): BomberArena =
      BomberArena(width, height, Location.orderedStream(width, height).map((_, Wall)).toMap)
  }

  case class BombPlacement(bomberArena: BomberArena) {
    import Entity.Bomb

    def maxRadius(location: Location): Int = {
      @tailrec
      def go(location: Location, radius: Int): Int = {
        val reachable = Direction.simpleDirections.map(location(_, radius))
        if (reachable.forall(x => !bomberArena.validLocation(x)))
          radius - 1
        else if (reachable.exists(x => !bomberArena.explosionAt(x)))
          radius - 1
        else
          go(location, radius + 1)
      }
      go(location, 1)
    }

    lazy val bombPlacements: Set[Block[Bomb]] =
      bomberArena.explosions.map(x => Block(x,Bomb(maxRadius(x)))).filterNot(_.entity.radius <= 0)
  }
}

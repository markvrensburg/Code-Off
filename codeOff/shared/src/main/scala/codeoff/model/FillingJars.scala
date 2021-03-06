package codeoff.model

object FillingJars {

  case class Liquid(kind: Int, amount: Int) {
    override val toString: String = s"$kind => $amount"
  }

  case class Jar(capacity: Int, filled: Option[Liquid], kinds: Set[Int]) {
    require(filled.map(_.amount).getOrElse(0) <= capacity, "Liquid amount greater than jar capacity")
    require(filled.map(_.kind).fold(true)(kinds.contains), "Liquid kind is not compatible with jar")

    lazy val amountFilled = filled.fold(0)(_.amount)
    lazy val space = capacity - amountFilled
    lazy val isEmpty = filled.isEmpty
    lazy val capability: Set[Int] =
      if (capacity > 0) filled.fold(kinds)(x => Set(x.kind)) else Set.empty
    lazy val fillable: Boolean = if (capacity > 0) isEmpty else false

    def isOfKind(kind: Int): Boolean =
      filled.fold(kinds.contains(kind))(kind == _.kind)

    def getAction(liquids: Liquids): Set[Int] =
      capability.intersect(liquids.getKinds)

    def fill(liquid: Liquid): (Jar, Liquid) = {
      if (isOfKind(liquid.kind)) {
        if (space >= liquid.amount)
          (Jar(capacity, Some(Liquid(liquid.kind ,amountFilled + liquid.amount)),kinds),Liquid(liquid.kind,0))
        else
          (Jar(capacity, Some(Liquid(liquid.kind ,capacity)),kinds),Liquid(liquid.kind,liquid.amount - space))
      }
      else
        (this, liquid)
    }
  }

  object Jar {
    def apply(capacity: Int, kinds: Set[Int]): Jar =
      Jar(capacity, None, kinds)
  }

  case class Liquids(liquids: Set[Liquid]) {

    lazy val totalAmount: Int = liquids.toList.map(_.amount).sum

    lazy val getKinds: Set[Int] = liquids.toList.map(_.kind).toSet

    def getKind(kind: Int): Option[Liquid] =
      liquids.find(_.kind == kind)

    def remove(liquid: Liquid): Liquids = {
      val updated = liquids.filter(_.kind == liquid.kind).map{ l =>
        if (liquid.amount >= l.amount)
          Liquid(liquid.kind, 0)
        else
          Liquid(liquid.kind, l.amount - liquid.amount)
      }
      Liquids((liquids.filterNot(_.kind == liquid.kind) ++ updated).filter(_.amount > 0))
    }

    def add(liquid: Liquid): Liquids = {
      if (liquid.amount > 0) {
        val updated = liquids.find(_.kind == liquid.kind).fold(liquid)(l => Liquid(l.kind, l.amount + liquid.amount))
        Liquids(liquids.filterNot(_.kind == liquid.kind) + updated)
      } else
        this
    }
  }

  object Liquids {
    def apply(liquids: Liquid*): Liquids =
      Liquids(Set(liquids: _*))
  }

  case class Jars(jars: Map[Int, Jar]) {

    lazy val fillable = jars.filter(_._2.fillable).toList

    def fill(jarID: Int, liquid: Liquid): (Jars, Liquid) =
      if (liquid.amount > 0) {
        jars.get(jarID).fold((this, liquid)) { j =>
          val (jar, liq) = j.fill(liquid)
          (Jars(jars.updated(jarID, jar)), liq)
        }
      } else
        (this, liquid)

    def sorted: List[Jar] = jars.toList.sortBy(_._1).map(_._2)
  }

  object Jars {
    def apply(jars: Jar*): Jars =
      Jars(List(jars: _*).zipWithIndex.map(x => (x._2, x._1)).toMap)
  }

  case class Fill(jarID: Int, liquidID: Int)

  case class FillingJars(liquids: Liquids, jars: Jars) {
    def apply(fill: Fill) = liquids.getKind(fill.liquidID).fold(this){ l =>
      val (filledJars, leftOverLiquid) = jars.fill(fill.jarID, l)
      FillingJars(liquids.remove(l).add(leftOverLiquid), filledJars)
    }

    lazy val remainder: Int = liquids.totalAmount

    lazy val actions: List[Fill] = jars.fillable.sortBy(_._1).headOption.fold(List.empty[Fill])(x =>
      x._2.getAction(liquids).map(y => Fill(x._1, y)).toList
    )
  }
}

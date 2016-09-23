package codeoff.core.search

import scalaz.std.AllInstances._
import scalaz.Order

case class InformedSearchNode[A, B](state: A, action: B, g: Double, h: Double) {
  lazy val eval = g + h
}

object InformedSearchNode {

  implicit def informedSearchNodeOrder[A,B]: Order[InformedSearchNode[A,B]] =
    Order.orderBy(_.eval)
}


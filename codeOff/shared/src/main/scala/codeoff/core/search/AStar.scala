package codeoff.core.search

import scala.collection.mutable
import scalaz.{TreeLoc, Order}

object AStar {

  def run[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): List[B] = {

    implicit val treeLocOrdering = Order[TreeLoc[InformedSearchNode[A,B]]].toScalaOrdering

    val open = mutable.PriorityQueue[TreeLoc[InformedSearchNode[A, B]]](tree.tree.loc)
    val closed = mutable.HashSet.empty[A]

    def retrievePlan(from: TreeLoc[InformedSearchNode[A, B]]): List[B] =
      from.path.map(_.action).reverse.tail.toList

    def successors[AA](node: TreeLoc[AA]): List[TreeLoc[AA]] = {
      def go(current: Option[TreeLoc[AA]], accum: List[TreeLoc[AA]]): List[TreeLoc[AA]] =
        current.fold(accum)(n => go(n.right, accum :+ n))
      go(node.firstChild, List.empty)
    }

    while (open.nonEmpty) {
      val current = open.dequeue()
      closed.add(current.getLabel.state)
      if (goal(current.getLabel.state))
        return retrievePlan(current)
      else {
        successors(current).foreach(n => {
          val state = n.getLabel.state
          if (!closed.contains(state)) {
            if (open.exists(_.getLabel.state == state))
              open.filterNot(x => (x.getLabel.state == state) && (x.getLabel.g > n.getLabel.g))
            else
              open.enqueue(n)
          }
        })
      }
    }
    List.empty
  }
}

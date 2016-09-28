package codeoff.core.search

import scala.collection.mutable
import scalaz.TreeLoc

object AStarMutable {

  def retrievePlan[A,B](from: TreeLoc[InformedSearchNode[A, B]]): List[B] =
    from.path.map(_.action).reverse.tail.toList

  def run[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): Option[TreeLoc[InformedSearchNode[A, B]]] = {

    implicit val treeLocOrdering = new Ordering[TreeLoc[InformedSearchNode[A,B]]] {
      override def compare(x: TreeLoc[InformedSearchNode[A, B]], y: TreeLoc[InformedSearchNode[A, B]]): Int = {
        Ordering[Double].compare(y.getLabel.eval, x.getLabel.eval)
      }
    }

    val open = mutable.PriorityQueue[TreeLoc[InformedSearchNode[A, B]]](tree.tree.loc)
    val closed = mutable.HashSet.empty[A]

    def successors[AA](node: TreeLoc[AA]): List[TreeLoc[AA]] = {
      def go(current: Option[TreeLoc[AA]], accum: List[TreeLoc[AA]]): List[TreeLoc[AA]] =
        current.fold(accum)(n => go(n.right, accum :+ n))
      go(node.firstChild, List.empty)
    }

    while (open.nonEmpty) {
      val current = open.dequeue()
      closed.add(current.getLabel.state)
      if (goal(current.getLabel.state))
        return Some(current)
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
    None
  }

  def runPlan[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): List[B] =
    run[A,B](tree, goal).fold(List.empty[B])(retrievePlan)

  def runState[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): Option[A] =
    run[A,B](tree, goal).map(_.getLabel.state)
}

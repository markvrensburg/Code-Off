package codeoff.core.search

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, HashSet}
import scalaz.{Heap, TreeLoc}

object AStar {

  def retrievePlan[A,B](from: TreeLoc[InformedSearchNode[A, B]]): List[B] =
    from.path.map(_.action).reverse.tail.toList

  def run[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): Option[TreeLoc[InformedSearchNode[A, B]]] = {

    def successors[AA](node: TreeLoc[AA]): List[TreeLoc[AA]] = {
      @tailrec
      def go(current: Option[TreeLoc[AA]], accum: List[TreeLoc[AA]]): List[TreeLoc[AA]] = current match {
        case Some(l) => go(l.right, accum :+ l)
        case None => accum
      }
      go(node.firstChild, List.empty)
    }

    @tailrec
    def go(open: Heap[TreeLoc[InformedSearchNode[A, B]]], closed: Set[A], cache: Map[A,Double]): Option[TreeLoc[InformedSearchNode[A, B]]] =
      open.uncons match {
        case Some((current, remainder)) =>
          if (goal(current.getLabel.state))
            Some(current)
          else {
            val candidate = successors(current).filterNot(x =>
              closed.contains(x.getLabel.state) || cache.get(x.getLabel.state).fold(false)(_ > x.getLabel.g))

            go(remainder.insertAll(candidate),
              closed + current.getLabel.state,
              cache ++ candidate.map(x => x.getLabel.state -> x.getLabel.g))
          }
        case _ => None
      }

    go(Heap.singleton(tree.tree.loc), HashSet.empty, HashMap(tree.tree.rootLabel.state -> tree.tree.rootLabel.g))
  }

  def runPlan[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): List[B] =
    run[A,B](tree, goal).fold(List.empty[B])(retrievePlan)

  def runState[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): Option[A] =
    run[A,B](tree, goal).map(_.getLabel.state)
}

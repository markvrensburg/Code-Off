package codeoff.core.search

import scala.annotation.tailrec
import scalaz.TreeLoc

object AStarImmutableSlow {

  def retrievePlan[A,B](from: TreeLoc[InformedSearchNode[A, B]]): List[B] =
    from.path.map(_.action).reverse.tail.toList

  def run[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): Option[TreeLoc[InformedSearchNode[A, B]]] = {

    def selectNode(nodes: Vector[TreeLoc[InformedSearchNode[A, B]]]): TreeLoc[InformedSearchNode[A, B]] =
      nodes.minBy(_.getLabel.eval)

    def successors[AA](node: TreeLoc[AA]): List[TreeLoc[AA]] = {
      @tailrec
      def go(current: Option[TreeLoc[AA]], accum: List[TreeLoc[AA]]): List[TreeLoc[AA]] = current match {
        case Some(l) => go(l.right, accum :+ l)
        case None => accum
      }
      go(node.firstChild, List.empty)
    }

    @tailrec
    def go(open: Vector[TreeLoc[InformedSearchNode[A, B]]], closed: Vector[TreeLoc[InformedSearchNode[A, B]]]): Option[TreeLoc[InformedSearchNode[A, B]]] = {
      if (open.nonEmpty) {
        val current = selectNode(open)
        if (goal(current.getLabel.state))
          Some(current)
        else {
          val (openMod, closedMod) = successors(current).foldLeft((open.filterNot(_ == current), closed :+ current)) { (a, n) => {
            val state = n.getLabel.state
            val openAcc = a._1
            val closedAcc = a._2
            if (closedAcc.exists(_.getLabel.state == state))
              (openAcc, closedAcc)
            else if (openAcc.exists(_.getLabel.state == state)) {
              (openAcc.filterNot(x => (x.getLabel.state == state) && (x.getLabel.g > n.getLabel.g)), closedAcc)
            }
            else
              (openAcc :+ n, closedAcc)
          }}
          go(openMod, closedMod)
        }
      } else
        None
    }
    go(Vector(tree.tree.loc), Vector.empty)
  }

  def runPlan[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): List[B] =
    run[A,B](tree, goal).fold(List.empty[B])(retrievePlan)

  def runState[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): Option[A] =
    run[A,B](tree, goal).map(_.getLabel.state)

}

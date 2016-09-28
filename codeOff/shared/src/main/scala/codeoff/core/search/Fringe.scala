package codeoff.core.search

import scala.annotation.tailrec
import scalaz.TreeLoc

object Fringe {

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
    def iterate(now: Vector[TreeLoc[InformedSearchNode[A, B]]], flimit: Double, later: Vector[TreeLoc[InformedSearchNode[A, B]]], fmin: Double): Either[(Vector[TreeLoc[InformedSearchNode[A, B]]], Double), Option[TreeLoc[InformedSearchNode[A, B]]]] = {
      if (now.nonEmpty) {
        val current = now.head
        val eval = {
          if (goal(current.getLabel.state))
            Right(Some(current))
          else {
            if (current.getLabel.eval > flimit)
              Left((Vector.empty, Vector(current), fmin.min(current.getLabel.eval)))
            else
              Left((
                successors(current).filterNot(n => now.exists(x => (x.getLabel.state == n.getLabel.state) && (x.getLabel.g <= n.getLabel.g))).toVector,
                Vector.empty, fmin
              ))
          }
        }
        eval match {
          case Right(path) => Right(path)
          case Left((toNow, toLater, min)) => iterate(toNow ++ now.tail, flimit, later ++ toLater, min)
        }
      }
      else
        Left((later, fmin))
    }

    @tailrec
    def go(fringe: Vector[TreeLoc[InformedSearchNode[A, B]]], flimit: Double): Option[TreeLoc[InformedSearchNode[A, B]]] = {
      if (fringe.nonEmpty) {
        val iter = iterate(fringe, flimit, Vector.empty, Double.MaxValue)
        iter match {
          case Right(path) => path
          case Left((nextFringe, limit)) => go(nextFringe, limit)
        }
      }
      else
        None
    }
    go(Vector(tree.tree.loc), tree.tree.loc.getLabel.h)
  }

  def runPlan[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): List[B] =
    run[A,B](tree, goal).fold(List.empty[B])(retrievePlan)

  def runState[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): Option[A] =
    run[A,B](tree, goal).map(_.getLabel.state)
}

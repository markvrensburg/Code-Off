package codeoff.core.search

import scala.annotation.tailrec
import scalaz.TreeLoc

object Fringe {

  def run[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): List[B] = {

    def retrievePlan(from: TreeLoc[InformedSearchNode[A, B]]): List[B] =
      from.path.map(_.action).reverse.tail.toList

    def successors[AA](node: TreeLoc[AA]): List[TreeLoc[AA]] = {
      def go(current: Option[TreeLoc[AA]], accum: List[TreeLoc[AA]]): List[TreeLoc[AA]] =
        current.fold(accum)(n => go(n.right, accum :+ n))
      go(node.firstChild, List.empty)
    }

    @tailrec
    def iterate(now: Vector[TreeLoc[InformedSearchNode[A, B]]], flimit: Double, later: Vector[TreeLoc[InformedSearchNode[A, B]]], fmin: Double): Either[(Vector[TreeLoc[InformedSearchNode[A, B]]], Double), List[B]] = {
      if (now.nonEmpty) {
        val current = now.head
        val eval = {
          if (goal(current.getLabel.state))
            Right(retrievePlan(current))
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
    def go(fringe: Vector[TreeLoc[InformedSearchNode[A, B]]], flimit: Double): List[B] = {
      if (fringe.nonEmpty) {
        val iter = iterate(fringe, flimit, Vector.empty, Double.MaxValue)
        iter match {
          case Right(path) => path
          case Left((nextFringe, limit)) => go(nextFringe, limit)
        }
      }
      else
        List.empty
    }

    go(Vector(tree.tree.loc), tree.tree.loc.getLabel.h)
  }
}

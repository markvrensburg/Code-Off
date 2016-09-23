package codeoff.core.search

import scala.annotation.tailrec
import scalaz.{Heap, TreeLoc}

object AStarImmutable {

  def run[A, B](tree: SearchTree[InformedSearchNode[A, B]], goal: Goal[A]): List[B] = {

    def retrievePlan(from: TreeLoc[InformedSearchNode[A, B]]): List[B] =
      from.path.map(_.action).reverse.tail.toList

    def successors[AA](node: TreeLoc[AA]): List[TreeLoc[AA]] = {
      def go(current: Option[TreeLoc[AA]], accum: List[TreeLoc[AA]]): List[TreeLoc[AA]] =
        current.fold(accum)(n => go(n.right, accum :+ n))
      go(node.firstChild, List.empty)
    }

    @tailrec
    def go(open: Heap[TreeLoc[InformedSearchNode[A, B]]], closed: Vector[TreeLoc[InformedSearchNode[A, B]]]): List[B] =
      open.uncons match {
        case Some((current, remainder)) =>
          if (goal(current.getLabel.state))
            retrievePlan(current)
          else {
            successors(current).filterNot(closed.exists)


            go(remainder.insertAll(successors(current)), closed :+ current)
          }
        case None => List.empty
      }

    /*
     if (open.nonEmpty) {
       val current = selectNode(open)
       if (goal(current.getLabel.state))
         retrievePlan(current)
       else {
         val (openMod, closedMod) = successors(current).foldLeft((open.filter(_ != current), closed :+ current)) { (a, n) => {
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
       List.empty
   }                  */


    go(Heap.singleton(tree.tree.loc), Vector.empty)
  }
}
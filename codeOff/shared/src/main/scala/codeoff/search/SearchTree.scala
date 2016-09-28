package codeoff.search

import scalaz.Tree

case class SearchTree[A](initial: A, transition: Transition[A]) {

  private[this] def expand(base: A): (A, () => Stream[A]) =
    (base, () => transition(base))

  lazy val tree: Tree[A] = Tree.unfoldTree(initial)(expand)
}

package codeoff.core

package object search {

  type CostFunction[A, B] = (A, B) => Double
  type Heuristic[A] = A => Double
  type Goal[A] = A => Boolean
  type Transition[A] = A => Stream[A]

  def NulHeuristic[A]: Heuristic[A] = a => 0
  def NulCost[A,B]: CostFunction[A,B] = (a,b) => 0
}

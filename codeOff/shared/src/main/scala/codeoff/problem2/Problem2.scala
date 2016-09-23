package codeoff.problem2

import codeoff.core.search._
import codeoff.problem2.Model.{Fill, FillingJars}

object Problem2 {

  def fillTransition(g: CostFunction[FillingJars, Fill], h: Heuristic[FillingJars]): Transition[InformedSearchNode[FillingJars, Fill]] =
    node => {
      node.state.actions.map(action => {
        val nextState = node.state(action)
        InformedSearchNode(nextState, action, node.g + g(node.state, action), h(nextState))
      })
    }.toStream

  val fillCost: CostFunction[FillingJars, Fill] = (fj, f) => (fj(f).minRemainder - fj.minRemainder).max(0)

  val fillGoal: Goal[FillingJars] = game => game.actions.isEmpty
}

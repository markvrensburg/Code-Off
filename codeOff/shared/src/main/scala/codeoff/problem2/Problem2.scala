package codeoff.problem2

import codeoff.core.FileIO
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

  val fillGoal: Goal[FillingJars] = _.actions.isEmpty

  def solve(input: FillingJars) = {
    val searchTree = SearchTree(InformedSearchNode(input, Fill(-1, -1), 0, NulHeuristic(input)), fillTransition(fillCost, NulHeuristic))
    AStar.run(searchTree, fillGoal)
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFile = filenames.filter(_.endsWith(".in")).head
    val text = io.readFileLines(s"$directory/$inputFile")
    val solution = Parser.parse(text.mkString(";")).get
    println(solution)
  }
}

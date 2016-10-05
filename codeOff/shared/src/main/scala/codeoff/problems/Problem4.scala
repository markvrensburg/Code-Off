package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Location
import codeoff.model.WaterSource.WaterSource
import codeoff.parser.WaterSourceParser
import codeoff.search.{AStarMutable, InformedSearchNode, SearchTree, _}

object Problem4 {

  val wallHeuristic: Heuristic[WaterSource] = _.waterRegions.size-1

  val wallCost: CostFunction[WaterSource, Location] = (_,_) => 1

  val wallGoal: Goal[WaterSource] = _.waterRegions.size == 1

  /*
  def transition(g: CostFunction[WaterSource, Location], h: Heuristic[WaterSource]): Transition[InformedSearchNode[WaterSource, Location]] =
    node => {
      node.state.actions.map(loc => {
        val nextState = node.state.removeWall(loc)
        InformedSearchNode(nextState, loc, node.g + g(node.state, loc), h(nextState))
      })
    }.toStream

  def solve(input: WaterSource) = {
    val initial = InformedSearchNode(input, Location(-1,-1), 0, wallHeuristic(input))
    val searchTree = SearchTree(initial, transition(wallCost, wallHeuristic))
    AStarMutable.runPlan(searchTree, wallGoal)
  }
  */

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = WaterSourceParser.parse(text.mkString).get

      println(state.waterEdges.size)
      println(state.wallEdges.size)


      val solution = state.removeWall(state.wallEdges)
      //val solution = state.mark(state.wallEdges)

      val solutionOutput = solution.draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

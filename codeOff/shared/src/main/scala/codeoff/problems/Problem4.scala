package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Location
import codeoff.model.WaterSource.WaterSource
import codeoff.parser.WaterSourceParser
import codeoff.search._

object Problem4 {

  //val wallHeuristic: Heuristic[(WaterSource, Vector[Set[Location]])] = x =>  (x._1.waterRegions.size-3).max(0)

  val wallGoal: Goal[(WaterSource, Vector[Set[Location]])] = _._1.waterRegions.size == 1

  val wallCost: CostFunction[(WaterSource, Vector[Set[Location]]), Set[Location]] = (_,l) => l.size

  val transition: Transition[InformedSearchNode[(WaterSource, Vector[Set[Location]]), Set[Location]]] =
    node => {
      node.state._2.map(locs => {
        val nextState = (node.state._1.removeWall(locs), node.state._2.filterNot(_ == locs))
        InformedSearchNode(nextState, locs, node.g + wallCost(node.state, locs),0)
      })
    }.toStream

  def solve(input: WaterSource) = {
    val waterEdge = input.waterEdges
    val regions = input.waterRegions.map(_.intersect(waterEdge))

    val actions = for {
      r1 <- regions
      r2 <- Vector(waterEdge.diff(r1))
    } yield input.shortestPath(r1,r2).toSet

    val initial = InformedSearchNode((input, actions), Set.empty[Location], 0, 0)
    val searchTree = SearchTree(initial, transition)

    //AStarMutable.runPlan(searchTree, wallGoal)

    actions
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in")).take(1)
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = WaterSourceParser.parse(text.mkString).get

      val solution = solve(state).toSet.flatten

      val solutionOutput = state.mark(solution).draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

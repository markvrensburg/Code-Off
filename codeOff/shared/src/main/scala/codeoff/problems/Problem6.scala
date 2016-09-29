package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Maze.Maze
import codeoff.model.{Distance, Location}
import codeoff.parser.MazeParser
import codeoff.search._

object Problem6 {

  def locationHeuristic(goals: Set[Location]): Heuristic[Maze] =
    _.current.fold(Double.MaxValue)(l => goals.map(g => Distance.manhattan(l, g)).min)

  val locationCost: CostFunction[Maze, Location] = (_,_) => 1

  def locationGoal(goals: Set[Location]): Goal[Maze] = _.current.fold(false)(goals.contains)

  def transition(g: CostFunction[Maze, Location], h: Heuristic[Maze]): Transition[InformedSearchNode[Maze, Location]] =
    node => {
      node.state.actions.map(loc => {
        val nextState = node.state(loc)
        InformedSearchNode(nextState, loc, node.g + g(node.state, loc), h(nextState))
      })
    }.toStream

  def solve(input: Maze) = {
    val goals = input.goals
    val h = locationHeuristic(goals)
    val initial = InformedSearchNode(input, input.current.get, 0, h(input))
    val searchTree = SearchTree(initial, transition(locationCost, h))
    AStarMutable.runPlan(searchTree, locationGoal(goals))
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))//.take(1)
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = MazeParser.parse(text.mkString).get
      val solution = solve(state)
      val solutionOutput = state.mark(solution.init.toSet).draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

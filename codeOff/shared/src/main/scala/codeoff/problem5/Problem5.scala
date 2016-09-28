package codeoff.problem5

import codeoff.core.FileIO
import codeoff.core.model.{Distance, Location}
import codeoff.core.model.Maze.Maze
import codeoff.core.parser.MazeParser
import codeoff.core.search._

object Problem5 {

  def locationHeuristic(goal: Location): Heuristic[Maze] =
    _.current.fold(Double.MaxValue)(l => Distance.manhattan(l, goal))

  val locationCost: CostFunction[Maze, Location] = (_,_) => 1

  def locationGoal(goal: Location): Goal[Maze] = _.current.fold(false)(_ == goal)

  def transition(g: CostFunction[Maze, Location], h: Heuristic[Maze]): Transition[InformedSearchNode[Maze, Location]] =
    node => {
      node.state.actions.map(loc => {
        val nextState = node.state(loc)
        InformedSearchNode(nextState, loc, node.g + g(node.state, loc), h(nextState))
      })
    }.toStream

  def solve(input: Maze) = {
    val goal = input.goals.head
    val h = locationHeuristic(goal)
    val initial = InformedSearchNode(input, input.current.get, 0, h(input))
    val searchTree = SearchTree(initial, transition(locationCost, h))
    AStarImmutable.runPlan(searchTree, locationGoal(goal))
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = MazeParser.parse(text.mkString).get
      val solutionOutput = state.mark(solve(state).init.toSet).draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

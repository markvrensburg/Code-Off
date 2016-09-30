package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Maze.Maze
import codeoff.model.{Distance, Location}
import codeoff.parser.MazeParser
import codeoff.search._

import scala.concurrent.Future

import monix.execution.Scheduler.Implicits.global

object Problem7 {

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

  def solve(input: Maze): Future[List[(Maze, List[Location])]] = {

    def solve(input: Maze, goal: Location): List[Location] = {
      val h = locationHeuristic(goal)
      val initial = InformedSearchNode(input, input.current.get, 0, h(input))
      val searchTree = SearchTree(initial, transition(locationCost, h))
      AStarMutable.runPlan(searchTree, locationGoal(goal))
    }

    val ends: List[Location] = input.sequentialGoals.sortBy(_._1).map(_._2)
    val starts: List[Location] = input.current.get :: ends.init
    val goals: List[(Location, Location)] = starts.zip(ends)

    Future.sequence(goals.map(x => {
      val state = input.assignCurrent(x._1)
      Future((state,solve(state, x._2)))
    }))
  }

  def run(directory: String, io: FileIO): Future[Unit] = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))

    Future.sequence(inputFiles.map{ file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = MazeParser.parse(text.mkString).get
      solve(state).map{solution =>
        val solutionOutput = solution.flatMap(x => x._1.mark(x._2.init.toSet).draw)
        io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
      }
    }).map(_ => ())
  }
}

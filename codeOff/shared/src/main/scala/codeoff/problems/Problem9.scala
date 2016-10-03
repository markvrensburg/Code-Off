package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Bomber.Entity.Bomb
import codeoff.model.Block
import codeoff.model.Bomber._
import codeoff.parser.BomberArenaParser
import codeoff.search._

object Problem9 {

  case class BombPlacementSearch(target: BomberArena) {

    val heuristic: Heuristic[(BomberArena, Set[Block[Bomb]])] = x =>
      target.explodeBombs.explosions.diff(x._1.explodeBombs.explosions).size

    val goal: Goal[(BomberArena, Set[Block[Bomb]])] = _._1.explodeBombs == target.explodeBombs

    val transition: Transition[InformedSearchNode[(BomberArena, Set[Block[Bomb]]), Block[Bomb]]] = node => {
        node.state._2.map(bloc => {
          val nextState = (node.state._1(bloc), node.state._2 - bloc)
          InformedSearchNode(nextState, bloc, 0, heuristic(nextState))
        })
      }.toStream
  }

  def solve(input: BomberArena) = {
    val search = BombPlacementSearch(input)
    val inputPlacements = (BomberArena.walls(input.width, input.height), BombPlacement(input).bombPlacements)
    //@TODO refactor InformedSearchNode, using null is a crime!!!!
    val initial = InformedSearchNode[(BomberArena, Set[Block[Bomb]]), Block[Bomb]](inputPlacements, null, 0, search.heuristic(inputPlacements))
    val searchTree = SearchTree(initial, search.transition)
    AStarMutable.runPlan(searchTree, search.goal)
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = BomberArenaParser.parse(text.mkString).get
      val solution = BomberArena.walls(state.width, state.height)(solve(state).toSet)
      val solutionOutput = solution.adjacentBonus(_ - _).draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

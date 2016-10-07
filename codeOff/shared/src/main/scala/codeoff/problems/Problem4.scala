package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Location
import codeoff.model.WaterSource.WaterSource
import codeoff.parser.WaterSourceParser

import scala.annotation.tailrec


object Problem4 {

  def solve(input: WaterSource): Set[Location] = {

    @tailrec
    def toBreak(depth: Int, ws: WaterSource, accum: Vector[Set[Location]]): Vector[Set[Location]] = {
      val edge = ws.wallEdgesDepth(depth)
      if (ws.waterRegions.size == 1)
        accum
      else if (edge.isEmpty)
        toBreak(depth + 1, ws, accum)
      else {
        val break = edge.maxBy(x => input.borders(x).size)
        toBreak(depth, ws.removeWall(break), accum :+ break)
      }
    }

    def removeRedundant(toBreak: Vector[Set[Location]]): Set[Location] = {
      val empty = input.removeWall(toBreak.toSet.flatten)
      toBreak.filterNot(x => empty.addWall(x).waterRegions.size == 1).toSet.flatten
    }

    removeRedundant(toBreak(1, input, Vector.empty))
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = WaterSourceParser.parse(text.mkString).get
      val solution = solve(state)
      val solutionOutput = state.mark(solution).draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

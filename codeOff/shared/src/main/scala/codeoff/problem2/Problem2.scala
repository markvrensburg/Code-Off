package codeoff.problem2

import codeoff.core.FileIO
import codeoff.core.search._
import codeoff.problem2.Model.FillingJars

object Problem2 {

  def solveBrute(input: FillingJars) = {
    val searchTree = SearchTree(input, (x: FillingJars) => x.actions.map(x(_)).toStream)
    searchTree.tree.flatten.filter(_.actions.isEmpty).minBy(_.remainder)
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFile = filenames.filter(_.endsWith(".in")).head
    val text = io.readFileLines(s"$directory/$inputFile")
    val state = Parser.parse(text.mkString(";")).get
    val solution = solveBrute(state)
    val solutionOutput = solution.remainder.toString :: solution.jars.sorted.map(j => s"${j.filled.fold("0")(x => s"${x.kind},${x.amount}")}")
    io.writeFileLines(s"$directory/${inputFile.replace(".in", ".out")}", solutionOutput)
  }
}

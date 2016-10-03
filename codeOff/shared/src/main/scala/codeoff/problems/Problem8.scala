package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.parser.BomberArenaParser


object Problem8 {

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = BomberArenaParser.parse(text.mkString).get
      val solutionOutput = state.adjacentBonus(_ + _).explodeBombs.draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

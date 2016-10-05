package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.parser.WaterSourceParser

object Problem3 {

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = WaterSourceParser.parse(text.mkString).get
      val solution= state.mark(state.waterRegions.maxBy(_.size))
      val solutionOutput = solution.draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

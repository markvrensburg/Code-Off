package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.parser.IntensityPlaneParser

object Problem13 {

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = IntensityPlaneParser.parse(text.mkString(" ")).get
      val solutionOutput = state.calculateIntensities.draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

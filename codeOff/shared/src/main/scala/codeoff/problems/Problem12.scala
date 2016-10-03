package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.parser.CardDeckParser


object Problem12 {

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = CardDeckParser.parse(text.mkString.filterNot(_.isSpaceChar)).get.asThunee
      println(state)
      /*val solutionOutput = ???
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)*/
    }
  }
}

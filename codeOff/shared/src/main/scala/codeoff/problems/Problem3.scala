package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Location
import codeoff.parser.WaterSourceParser

object Problem3 {

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = WaterSourceParser.parse(text.mkString).get
      val solution= state.mark{
        val regions = state.waterRegions
        val largestSize = regions.maxBy(_.size).size
        regions.filter(_.size == largestSize).foldLeft(Set.empty[Location])(_ ++ _)
      }
      val solutionOutput = solution.draw
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

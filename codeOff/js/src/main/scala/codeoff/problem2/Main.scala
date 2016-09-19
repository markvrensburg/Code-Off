package codeoff.problem2

import scala.scalajs.js
import codeoff.core.FileIO
import codeoff.problem1.Problem1

object Main extends js.JSApp {

  def main(): Unit = {
    val directory = "code_off-1"
    val filenames = FileIO.listFiles(directory)
    val inputFile = filenames.filter(_.endsWith(".in")).head
    val text = FileIO.readFile(s"$directory/$inputFile").split(FileIO.EOL).toList
    println(Problem1.makeOutput(text).filter(_._3.nonEmpty).map(_._3).size)
  }
}

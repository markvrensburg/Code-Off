package codeoff.problem2

import codeoff.problems.Problem2
import codeoff.fileio.FileIOJS

import scala.scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {
    Problem2.run("code_off-2", FileIOJS)
  }
}

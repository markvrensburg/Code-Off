package codeoff.problem1

import codeoff.problems.Problem1
import codeoff.fileio.FileIOJS

import scala.scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {
    Problem1.run("code_off-1", FileIOJS)
  }
}

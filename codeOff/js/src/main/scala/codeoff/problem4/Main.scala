package codeoff.problem4

import codeoff.problems.Problem4
import codeoff.fileio.FileIOJS

import scala.scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {
    Problem4.run("code_off-4", FileIOJS)
  }
}

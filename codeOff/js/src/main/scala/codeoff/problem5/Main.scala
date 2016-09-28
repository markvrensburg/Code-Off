package codeoff.problem5

import codeoff.problems.Problem5
import codeoff.fileio.FileIOJS

import scala.scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {
    Problem5.run("code_off-5", FileIOJS)
  }
}

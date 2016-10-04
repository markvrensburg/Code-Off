package codeoff.problem13

import codeoff.problems.Problem13
import codeoff.fileio.FileIOJS

import scala.scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {
    Problem13.run("code_off-13", FileIOJS)
  }
}

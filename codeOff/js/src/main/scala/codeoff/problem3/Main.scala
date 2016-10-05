package codeoff.problem3

import codeoff.problems.Problem3
import codeoff.fileio.FileIOJS

import scala.scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {
    Problem3.run("code_off-3", FileIOJS)
  }
}

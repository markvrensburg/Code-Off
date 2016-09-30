package codeoff.problem7

import codeoff.problems.Problem7
import codeoff.fileio.FileIOJS

import scala.scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {
    Problem7.run("code_off-7", FileIOJS)
    ()
  }
}

package codeoff.problem7

import codeoff.problems.Problem7
import codeoff.fileio.FileIOJVM

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {

  def main(args: Array[String]): Unit = {
    Await.ready(Problem7.run("code_off-7", FileIOJVM), Duration.Inf)
  }
}


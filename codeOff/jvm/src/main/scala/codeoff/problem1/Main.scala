package codeoff.problem1

import codeoff.core.problem1.Problem1._

object Main {

  def main(args: Array[String]) = {
    val text = for {
      ioDir <- ioArgs(args)
      inputFile <- getListOfFiles(ioDir, ".in")
      //outputFile <-
      input <- readFile(inputFile.head)
    //move <- solve(parsedGame, botKey)
    //_ <- writeFile(outputFile, move.toString)
    } yield input
    println(text.get)
  }
}

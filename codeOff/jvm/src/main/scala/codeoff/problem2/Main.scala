package codeoff.problem2

import codeoff.problem1.Problem1._

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
    println(makeOutput(text.get).filter(_._3.nonEmpty).map(_._3).size)
    println((value("zGvWDMZk"),value("cqELWZoiW")))
  }
}

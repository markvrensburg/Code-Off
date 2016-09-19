package codeoff.problem1

import Problem1._

object Main {

  def main(args: Array[String]) = {

    val text = for {
      ioDir <- ioArgs(args)
      inputFile <- getListOfFiles(ioDir, ".json")
      //outputFile <-
      input <- readFile(inputFile.head)
      //move <- solve(parsedGame, botKey)
      //_ <- writeFile(outputFile, move.toString)
    } yield input.mkString("\n")
    println(parseJson(text.get))
  }
}

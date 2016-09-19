package codeoff.problem1

import scala.scalajs.js
import codeoff.core.{ProblemLocations, ProblemLocation, FileIO}

import io.circe.syntax._
import io.circe.generic.auto._

object Main extends js.JSApp {
  def main(): Unit = {
    val filenames = FileIO.listFiles(".")
    val inputFile = filenames.filter(_.endsWith(".json")).head
    val parsed = Problem1.parseJson(FileIO.readFile(inputFile))
    println(parsed.getOrElse(ProblemLocations(List(ProblemLocation(0,"")))).asJson.noSpaces)
  }
}

package codeoff.problem2

import codeoff.core.{FileIOJVM, JsonUtil, ProblemLocation, ProblemLocations}

import io.circe.syntax._
import io.circe.generic.auto._

object Main {
  def main(args: Array[String]) = {
    val filenames = FileIOJVM.listFiles(".")
    val inputFile = filenames.filter(_.endsWith(".json")).head
    val parsed = JsonUtil.parseJson(FileIOJVM.readFile(inputFile))
    println(parsed.getOrElse(ProblemLocations(List(ProblemLocation(0, "")))).asJson.noSpaces)
  }
}

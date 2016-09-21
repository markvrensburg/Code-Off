package codeoff.problem2

import scala.scalajs.js
import codeoff.core.{FileIOJS, JsonUtil, ProblemLocation, ProblemLocations}
import io.circe.syntax._
import io.circe.generic.auto._


object Main extends js.JSApp {
  def main(): Unit = {
    val filenames = FileIOJS.listFiles(".")
    val inputFile = filenames.filter(_.endsWith(".json")).head
    val parsed = JsonUtil.parseJson(FileIOJS.readFile(inputFile))
    println(parsed.getOrElse(ProblemLocations(List(ProblemLocation(0, "")))).asJson.noSpaces)
  }
}

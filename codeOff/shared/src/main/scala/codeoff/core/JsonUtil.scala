package codeoff.core

import io.circe.Json
import io.circe.parser._
import io.circe.generic.auto._

object JsonUtil {

  def parseJson(input: String) = {
    val json = parse(input).getOrElse(Json.Null)
    json.as[ProblemLocations]
  }
}

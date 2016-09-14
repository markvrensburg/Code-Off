package codeoff.problem1

import scala.scalajs.js
import js.Dynamic.global

object Main extends js.JSApp {

  val fs = global.require("fs")

  def main(): Unit = {
    val filenames = listFiles("./code_off-1")
    println(filenames)
  }

  def listFiles(path: String): Seq[String] = {
    fs.readdirSync(path).asInstanceOf[js.Array[String]]
  }
}

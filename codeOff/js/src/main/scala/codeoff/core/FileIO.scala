package codeoff.core

import scala.scalajs.js
import scala.scalajs.js.Dynamic._

object FileIO {

  val fs = global.require("fs")
  val os = global.require("os")

  val EOL = os.EOL.asInstanceOf[String]

  def listFiles(path: String): Seq[String] = {
    fs.readdirSync(path).asInstanceOf[js.Array[String]]
  }

  def readFile(path: String): String = {
    //val options = js.Dynamic.literal(encoding = "UTF-8")
    fs.readFileSync(path).asInstanceOf[String]
  }

  def writeFile(path: String, data: String): Unit = {
    //val options = js.Dynamic.literal(encoding = "UTF-8")
    fs.writeFileSync(path, data)
    ()
  }
}

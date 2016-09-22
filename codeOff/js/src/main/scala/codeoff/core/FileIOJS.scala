package codeoff.core

import scala.scalajs.js
import scala.scalajs.js.Dynamic._

object FileIOJS extends FileIO {

  private val fs = global.require("fs")
  private val os = global.require("os")

  override val EOL = os.EOL.asInstanceOf[String]

  private val options = js.Dynamic.literal(encoding = "UTF-8", flag = "w")

  override def listFiles(path: String): List[String] =
    fs.readdirSync(path).asInstanceOf[js.Array[String]].toList

  override def readFileLines(path: String): List[String] =
    readFile(path).split(EOL).toList

  override def readFile(path: String): String =
    fs.readFileSync(path).asInstanceOf[String]

  override def writeFileLines(path: String, data: List[String]): Unit = {
    writeFile(path, data.mkString(EOL))
    ()
  }

  override def writeFile(path: String, data: String): Unit = {
    fs.writeFileSync(path, data, options)
    ()
  }
}

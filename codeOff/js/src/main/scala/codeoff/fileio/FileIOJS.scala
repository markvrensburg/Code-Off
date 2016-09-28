package codeoff.fileio

import scala.scalajs.js

object FileIOJS extends FileIO {

  private val fs = js.Dynamic.global.require("fs")
  private val os = js.Dynamic.global.require("os")

  override val EOL = os.EOL.asInstanceOf[String]

  private val options = js.Dynamic.literal(encoding = "UTF-8", flag = "w")

  override def listFiles(path: String): List[String] =
    fs.readdirSync(path).asInstanceOf[js.Array[String]].toList

  private def readFile(path: String): String =
    fs.readFileSync(path).asInstanceOf[String]

  override def readFileLines(path: String): List[String] =
    readFile(path).replace("\r","").split("\n").toList

  override def writeFileLines(path: String, data: List[String]): Unit = {
    writeFile(path, data.mkString(EOL))
    ()
  }

  private def writeFile(path: String, data: String): Unit = {
    fs.writeFileSync(path, data, options)
    ()
  }
}

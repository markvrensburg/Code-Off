package codeoff.fileio

trait FileIO {

  val EOL: String

  def listFiles(path: String): List[String]

  def readFileLines(path: String): List[String]

  def writeFileLines(path: String, data: List[String]): Unit
}

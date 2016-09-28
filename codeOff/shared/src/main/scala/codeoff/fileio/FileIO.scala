package codeoff.fileio

trait FileIO {

  val EOL: String

  def listFiles(path: String): List[String]

  def readFile(path: String): String

  def readFileLines(path: String): List[String]

  def writeFile(path: String, data: String): Unit

  def writeFileLines(path: String, data: List[String]): Unit
}

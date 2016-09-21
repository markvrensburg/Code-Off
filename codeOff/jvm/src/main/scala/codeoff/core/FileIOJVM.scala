package codeoff.core

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.io.Source

object FileIOJVM extends FileIO {

  override val EOL: String = System.lineSeparator

  private def resolvePath(path: String): Path = Paths.get(path)

  override def listFiles(path: String): List[String] =
    resolvePath(path).toFile.listFiles.toList.map(_.getName)

  override def readFile(path: String): String =
    readFileLines(path).mkString(EOL)

  override def readFileLines(path: String): List[String] = {
    val source = Source.fromFile(resolvePath(path).toFile)(StandardCharsets.UTF_8)
    source.getLines.toList
  }

  override def writeFile(path: String, data: String): Unit = {
    Files.write(resolvePath(path), data.getBytes(StandardCharsets.UTF_8))
    ()
  }

  override def writeFileLines(path: String, data: List[String]): Unit = {
    writeFile(path, data.mkString(EOL))
    ()
  }
}

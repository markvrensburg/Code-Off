package codeoff.core.problem1

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file._

import cats._
import cats.implicits._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Problem1 {

  def charValue(c: Char): Option[Int] = {
    if (c.isLower) Some(c - 97)
    else if (c.isUpper) Some((c - 65) + 1)
    else None
  }

  def value(s: String) = s.map(charValue).toList.sequence.map(_.sum)

  def palindrome(s: String): Boolean = s == s.reverse

  @tailrec
  def palindromeRec(s: String): Boolean = {
    if (s.length <= 1) true
    else {
      if (s.head == s.last) palindromeRec(s.init.tail)
      else false
    }
  }

  def equalValue(s1: String, s2: String): Boolean = value(s1) == value(s2)

  def findEquals(s: String, ls: List[String]): List[String] = ls.filter(equalValue(s,_))

  def accumulateEquals(ls: List[String]): Map[Option[Int],List[String]] = {
    val mapped: List[Map[Option[Int],List[String]]] = ls.map(x => Map(value(x) -> List(x)))
    Monoid[Map[Option[Int],List[String]]].combineAll(mapped)
  }

  def ioArgs(args: Array[String]): Try[Path] = Try {
    Paths.get(args(0))
  }

  def getListOfFiles(dir: Path, extension: String): Try[List[Path]] = Try {
    dir.toFile.listFiles.filter(file => file.isFile && file.getName.endsWith(extension)).toList.map(_.toPath)
  }

  def readFile(path: Path): Try[String] = Try {
    val source = Source.fromFile(path.toFile)(StandardCharsets.UTF_8)
    source.getLines().mkString("\n")
  }

  def writeFile(path: Path, output: String): Try[Unit] = Try {
    Files.write(path, output.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
    ()
  }
}

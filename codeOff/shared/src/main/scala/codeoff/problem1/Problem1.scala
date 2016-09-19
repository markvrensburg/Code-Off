package codeoff.problem1

import java.nio.charset.StandardCharsets
import java.nio.file._

import cats._
import cats.implicits._
import codeoff.core.ProblemLocations
import io.circe.Json
import io.circe.parser._
import io.circe.generic.auto._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Problem1 {

  def palindrome(s: String): Boolean = s == s.reverse

  @tailrec
  def palindromeRec(s: String): Boolean = {
    if (s.length <= 1) true
    else {
      if (s.head == s.last) palindromeRec(s.init.tail)
      else false
    }
  }

  def charValue(c: Char): Option[Int] = {
    if (c.isLower) Some(c - 97)
    else if (c.isUpper) Some((c - 65) + 1)
    else None
  }

  def value(s: String): Option[Int] = s.map(charValue).toList.sequence.map(_.sum)

  def equalValue(s1: String, s2: String): Boolean = value(s1) == value(s2)

  def findEquals(s: String, ls: List[String]): List[String] = ls.filter(equalValue(s,_))

  def accumulateEquals(ls: List[String]): Map[Int, List[String]] = {
    val mapped = ls.map(x => value(x).fold(Map.empty[Int, List[String]])(v => Map(v -> List(x))))
    Monoid[Map[Int,List[String]]].combineAll(mapped)
  }

  def ioArgs(args: Array[String]): Try[Path] = Try {
    if (args.isEmpty) Paths.get(".") else Paths.get(args(0))
  }

  def getListOfFiles(dir: Path, extension: String): Try[List[Path]] = Try {
    dir.toFile.listFiles.filter(file => file.getName.endsWith(extension)).toList.map(_.toPath)
  }

  def readFile(path: Path): Try[List[String]] = Try {
    val source = Source.fromFile(path.toFile)(StandardCharsets.UTF_8)
    source.getLines().toList
  }

  def writeFile(path: Path, output: String): Try[Unit] = Try {
    Files.write(path, output.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
    ()
  }

  def parseJson(input: String) = {
    val json = parse(input).getOrElse(Json.Null)
    json.as[ProblemLocations]
  }

  def makeOutput(ls: List[String]) = {
    val a = accumulateEquals(ls)
    ls.map(x => (x, palindrome(x), value(x).fold(List.empty[String])(v => a.get(v).fold(List.empty[String])(_.diff(List(x))))))
  }
}

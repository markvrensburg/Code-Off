package codeoff.problem11

import cats.implicits._
import codeoff.core.FileIO
import codeoff.core.data.MorseCode

object Problem11 {

  import MorseCode._

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFile = filenames.filter(_.endsWith(".in")).head
    val text = io.readFileLines(s"$directory/$inputFile")
    val solution: List[String] = solve(text)
    io.writeFileLines(s"$directory/${inputFile.replace(".in", ".out")}", solution)
  }

  def solve(input: List[String]): List[String] =
    input.flatMap(x => decodeWords(x).fold(List.empty[String])(y => List(y.mkString(" "))))

  def decodeWords(s: String): Option[List[String]] =
    s.split('/').map(decodeWord).toList.sequence

  def decodeWord(s: String): Option[String] =
    s.split('|').map(decodeMorse(_).flatMap(values.map({ case (k, v) => (v, k) }).get)).toList.sequence.map(_.mkString)

  def decodeMorse(s: String): Option[String] = {
    def valueOf(v: Char): Option[String] = {
      if (v.isDigit) Some("." * v.toString.toInt)
      else if (v.isLetter) Some("-" * (v.toUpper - 'A' + 1))
      else None
    }
    s.map(valueOf).toList.sequence.map(_.mkString)
  }
}

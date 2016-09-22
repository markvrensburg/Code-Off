package codeoff.problem10

import cats.implicits._
import codeoff.core.FileIO

import scala.annotation.tailrec

object Problem10 {

  import codeoff.core.MorseCode._

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFile = filenames.filter(_.endsWith(".in")).head
    val text = io.readFileLines(s"$directory/$inputFile")
    val solution: List[String] = solve(text)
    io.writeFileLines(s"$directory/${inputFile.replace(".in", ".out")}", solution)
  }

  def solve(input: List[String]) =
    input.map(x => encodeWords(x.split(' ').toList)).sequence.fold(List.empty[String])(identity)

  def encodeWords(s: List[String]): Option[String] =
    s.map(encodeWord).sequence.map(_.mkString("/"))

  def encodeWord(s: String): Option[String] = {
    s.map(values.get).toList.sequence.map(_.map(encodeMorse))
      .map(_.sequence).flatten.map(_.mkString("|"))
  }

  def encodeMorse(s: String): Option[String] = {
    def valueOf(v: (Int, Char)): Option[String] = v match {
      case (n, '.') => Some(n.toString)
      case (n, '-') => Some(('A' + (n - 1)).toChar.toString)
      case _ => None
    }

    @tailrec
    def loop(input: String, last: Option[Char], accum: List[(Int, Char)]): List[(Int, Char)] = {
      input.headOption match {
        case Some(h) => loop(input.tail, Some(h), {
          if (last.fold(false)(_ == h))
            accum.init :+ ((accum.last._1 + 1, h))
          else
            accum :+ ((1, h))
        })
        case None => accum
      }
    }

    loop(s, None, Nil).map(valueOf).sequence.map(_.mkString)
  }
}

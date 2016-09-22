package codeoff.problem1

import cats._
import cats.implicits._
import codeoff.core.FileIO

import scala.annotation.tailrec

object Problem1 {

  @tailrec
  def palindromeRec(s: String): Boolean = {
    if (s.length <= 1) true
    else {
      if (s.head == s.last) palindromeRec(s.init.tail)
      else false
    }
  }

  def findEquals(s: String, ls: List[String]): List[String] =
    ls.filter(equalValue(s, _))

  def equalValue(s1: String, s2: String): Boolean =
    value(s1) == value(s2)

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFile = filenames.filter(_.endsWith(".in")).head
    val text = io.readFileLines(s"$directory/$inputFile").drop(1)
    val solution: List[String] = solve(text).flatMap(x =>
      List(x._1, x._2.toString) ++ x._3).filterNot(_.isEmpty)
    io.writeFileLines(s"$directory/${inputFile.replace(".in", ".out")}", solution)
  }

  def solve(input: List[String]) = {
    val a = accumulateEquals(input)
    input.map(x => (x, palindrome(x), value(x).fold(List.empty[String])(v => a.get(v).fold(List.empty[String])(_.diff(List(x))))))
  }

  def palindrome(s: String): Boolean =
    s == s.reverse

  def accumulateEquals(ls: List[String]): Map[Int, List[String]] = {
    val mapped = ls.map(x => value(x).fold(Map.empty[Int, List[String]])(v => Map(v -> List(x))))
    Monoid[Map[Int, List[String]]].combineAll(mapped)
  }

  def value(s: String): Option[Int] =
    s.map(charValue).toList.sequence.map(_.sum)

  def charValue(c: Char): Option[Int] = {
    if (c.isLower) Some(c - 97)
    else if (c.isUpper) Some((c - 65) + 1)
    else None
  }
}

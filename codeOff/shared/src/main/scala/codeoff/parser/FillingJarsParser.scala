package codeoff.parser

import codeoff.model.FillingJars._
import fastparse.all._

import scala.util.Try

object FillingJarsParser {

  private[this] val EOL = ";"

  private[this] val number: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))

  private[this] val liquids = P(
    (for {
      numLiquids <- P(number ~ EOL)
      liquids <- P((number ~ EOL.?).rep(exactly=numLiquids).map(_.zipWithIndex.map(x => Liquid(x._2, x._1))))
    } yield liquids).map(x => Liquids(x: _*))
  )

  private[this] val jars = P(
    (for {
      numJars <- P(number ~ EOL)
      jars <- P((number.rep(sep=",").map(x => Jar(x.head, x.tail.toSet)) ~ EOL.?).rep(exactly=numJars))
    } yield jars).map(x => Jars(x: _*))
  )

  private[this] val fillingJars = P((liquids ~ jars).map(x => FillingJars(x._1, x._2)))

  def parse(input: String): Try[FillingJars] =
    Try(fillingJars.parse(input).get.value)
}



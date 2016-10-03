package codeoff.parser

import codeoff.model.Cards._
import fastparse.all._

import scala.util.Try

object CardDeckParser {

  private[this] val rank = P(("10" | CharIn("JQK") | CharIn('1'to'9')).!.map(x => CardRank.read(x)))

  private[this] val suit = P(CharIn("SCHD").!.map(x => CardSuit.read(x)))

  private[this] val card = P((rank ~ suit).map(x => Card(x._1, x._2)))

  val deck = P(card.rep.map(x => Deck(x.toSet)))

  def parse(input: String): Try[Deck] = Try(deck.parse(input).get.value)
}

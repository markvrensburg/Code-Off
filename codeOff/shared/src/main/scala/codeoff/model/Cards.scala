package codeoff.model

object Cards {

  case class CardRank(value: Int) {
    require((value >= 1) && (value <= 13), s"Invalid Card Rank: $value => Not within 1 and 13")

    override val toString = value match {
      case 13 => "K"
      case 12 => "Q"
      case 11 => "J"
      case r => r.toString
    }
  }

  object CardRank {
    def read(s: String): CardRank = s match {
      case "K" => CardRank(13)
      case "Q" => CardRank(12)
      case "J" => CardRank(11)
      case _ => CardRank(s.toInt)
    }
  }

  sealed trait CardSuit
  case object Spades extends CardSuit {
    override val toString = "S"
  }
  case object Clubs extends CardSuit {
    override val toString = "C"
  }
  case object Hearts extends CardSuit {
    override val toString = "H"
  }
  case object Diamonds extends CardSuit {
    override val toString = "D"
  }

  case class Card(rank: CardRank, suit: CardSuit) {
    override val toString = s"$rank$suit"
  }

  object CardSuit {
    def read(s: String): CardSuit = s match {
      case "S" => Spades
      case "C" => Clubs
      case "H" => Hearts
      case "D" => Diamonds
    }
  }

  case class Deck(cards: Set[Card]) {

    lazy val asThunee: Deck = Deck(cards.filterNot(x => (x.rank.value >= 2) && (x.rank.value <= 8)))
  }
}

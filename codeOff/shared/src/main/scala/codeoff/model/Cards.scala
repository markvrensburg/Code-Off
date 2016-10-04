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

  sealed trait CardSuit {
    lazy val value: Int = this match {
      case Spades => 1
      case Clubs => 2
      case Hearts => 3
      case Diamonds => 4
    }
  }

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

  object Card {

    implicit val cardOrdering = new Ordering[Card] {
      override def compare(c1: Card, c2: Card): Int = {
        if (c1.rank.value > c2.rank.value) 1
        else if (c1.rank.value == c2.rank.value) {
          if (c1.suit.value > c2.suit.value) 1
          else if (c1.suit.value == c2.suit.value) 0
          else -1
        }
        else -1
      }
    }
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

    def contains(card: Card): Boolean = cards.contains(card)

    def remove(card: Card): Deck = Deck(cards.filterNot(_ == card))
  }

  case class Hand(playerId: Int, cards: Set[Card]) {
    def deal(card: Card): Hand = Hand(playerId, cards + card)

    lazy val orderCards: List[Card] = cards.toList.sorted

    override val toString = s"Hand($playerId => ${orderCards.mkString(",")}"
  }

  case class DealState(hands: Set[Hand], deck: Deck, dealTo: Int) {

    def deal(card: Card, numPlayers: Int): DealState = {
      if (deck.contains(card)) {
        hands.find(_.playerId == dealTo).fold(this)(hand => {
          val nextPlayer = if ((dealTo + 1) > numPlayers) 1 else dealTo + 1
          DealState(hands.filterNot(_.playerId == dealTo) + hand.deal(card), deck.remove(card), nextPlayer)
        })
      }
      else
        this
    }
  }

  object Hands {
    def orderd(hands: Set[Hand]) = hands.toList.sortBy(_.playerId).map(_.orderCards)

    def show(hands: Seq[Seq[Card]]): String = hands.map(_.mkString(",")).mkString("|")

    def show(hands: Set[Hand]): String = hands.toSeq.sortBy(_.playerId).map(_.orderCards.mkString(",")).mkString("|")
  }
}

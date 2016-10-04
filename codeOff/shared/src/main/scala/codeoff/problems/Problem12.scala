package codeoff.problems

import codeoff.fileio.FileIO
import codeoff.model.Cards._
import codeoff.parser.CardDeckParser
import codeoff.search.SearchTree
import spire.math._

import scala.collection.immutable.HashSet

object Problem12 {

  def combinations(deckSize: Int, numPlayers: Int, handSize: Int) =
    (0 until numPlayers).map(x => deckSize - (x * handSize)).map(y => fact(y)/(fact(y-handSize)*fact(handSize))).product

  def solveTree(deck: Deck, numPlayers: Int, deals: Int) = {
    val hands = (1 to numPlayers).map(Hand(_, HashSet.empty)).toSet
    val searchTree = SearchTree(DealState(hands, deck, 1), (x: DealState) => x.deck.cards.map(c => x.deal(c, numPlayers)).toStream)
    searchTree.tree.levels(deals).map(_.hands)
  }

  def solve(deck: Deck) = {

    def hand(deck: Deck) = for {
      c1 <- deck.cards
      c2 <- deck.cards - c1
      c3 <- deck.cards - c1 - c2
      c4 <- deck.cards - c1 - c2 - c3
    } yield HashSet(c1, c2, c3, c4)

    def deal(deck: Deck) = for {
      d1 <- hand(deck)
      d2 <- hand(Deck(deck.cards -- d1))
      d3 <- hand(Deck(deck.cards -- d1 -- d2))
      d4 <- hand(Deck(deck.cards -- d1 -- d2 -- d3))
    } yield Vector(d1, d2, d3, d4)

    deal(deck)
  }

  def run(directory: String, io: FileIO): Unit = {
    val filenames = io.listFiles(directory)
    val inputFiles = filenames.filter(_.endsWith(".in"))
    inputFiles.foreach{file =>
      val text = io.readFileLines(s"$directory/$file")
      val state = CardDeckParser.parse(text.mkString.filterNot(_.isSpaceChar)).get.asThunee
      val solution = solveTree(state,4,16)
      val solutionOutput =
        s"This file should contain ${combinations(state.cards.size,4,4)} entries..." ::
        solution.map(Hands.orderd).take(100).toList.distinct.map(Hands.show) ++
        List("...")
      io.writeFileLines(s"$directory/${file.replace(".in", ".out")}", solutionOutput)
    }
  }
}

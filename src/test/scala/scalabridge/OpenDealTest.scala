package scalabridge

import org.junit.jupiter.api.Test
import scalabridge.events.Event
import scalabridge.events.PlayCardEvent
import scalabridge.nonpure.DuplicateBoardBuilder

import java.time.Instant

@Test
class OpenDealTest extends UnitFunSpec {

  private def getSuit(s: String): Suit = {
    Suit.values
      .find(suit => suit.getName.toLowerCase.charAt(0) == s.toLowerCase.charAt(0))
      .getOrElse(Suit.SPADES)
  }

  private def getRank(s: String): Rank = {
    val rankString = ("" + s.charAt(1)).toLowerCase
    Rank.values
      .find(rank => rank.getSymbol.toLowerCase == rankString)
      .getOrElse(Rank.ACE)
  }

  private def findDirection(c: Card, board: DuplicateBoard): Direction = {
    Direction.values
      .find(direction => board.getHandOf(direction).containsCard(c))
      .getOrElse(Direction.NORTH)
  }

  private def assertEventIsCorrect(expectedCardString: String, event: Event) = {
    event shouldBe a[PlayCardEvent]
    val expectedCard = Card(getSuit(expectedCardString), getRank(expectedCardString))
    val actualCard = event.asInstanceOf[PlayCardEvent].card
    actualCard shouldBe expectedCard
  }

  describe("An OpenDeal") {
    it("should return its deal actions in first to last order") {
      val boardNumber: Int = 2
      val pbnDealTag: String =
        "S:J.QJ953.K43.AT52 98765.A.QJ9.K964 QT43.T8762.AT.J7 AK2.K4.87652.Q83"
      val validDuplicateBoard: DuplicateBoard =
        DuplicateBoardBuilder.build(boardNumber, pbnDealTag)
      val listOfCardStrings = List(
        "h2",
        "h4",
        "hJ",
        "hA",
        "s5",
        "s3",
        "sA",
        "sJ",
        "d2",
        "d4",
        "dQ",
        "dA",
        "h8",
        "hK",
        "h9",
        "c4",
        "d5",
        "dK",
        "d9",
        "dT",
        "d3",
        "dJ",
        "s4",
        "d6",
        "sQ",
        "sK",
        "h3",
        "s6",
        "d8",
        "c2",
        "c6",
        "sT",
        "cJ",
        "c3",
        "cA",
        "c9"
      )
      val listOfCards =
        listOfCardStrings.map((cardString: String) =>
          Card(getSuit(cardString), getRank(cardString))
        )

      var openDeal: OpenDeal = OpenDeal.empty(validDuplicateBoard)
      for (card <- listOfCards) {
        openDeal = openDeal.addEvent(
          PlayCardEvent(Instant.now, findDirection(card, validDuplicateBoard), card)
        )
      }
      val dealEvents = openDeal.getDealEvents
      listOfCardStrings.zipWithIndex.foreach((expectedCard, index) =>
        assertEventIsCorrect(expectedCard, dealEvents(index))
      )
    }

  }
}

package scalabridge

import org.scalatest.flatspec.AnyFlatSpec
import org.junit.jupiter.api.Test
import scalabridge.nonpure.ContractFromTextValidatedBuilder
import scala.collection.immutable.SortedSet

@Test
class HandTest extends AnyFlatSpec {
  private val validHandString = "KJT932.97.942.86"
  private val completeHand = CompleteHand(validHandString)
  private val cardsInHand = completeHand.cards

  val aceOfSpades = Card(Suit.SPADES, Rank.ACE)
  val tenOfSpades = Card(Suit.SPADES, Rank.TEN)
  val threeOfSpades = Card(Suit.SPADES, Rank.THREE)
  val kingOfClubs = Card(Suit.CLUBS, Rank.KING)

  "A Hand" should "be constructable from java list" in {
    val javaList: java.util.List[Card] = java.util.List.of(aceOfSpades,tenOfSpades)
    val myHand = Hand(javaList)
    assertResult(Hand(Set(aceOfSpades,tenOfSpades)))(myHand)
  }

  "A Hand" should "return its cards sorted by its ordering" in {
    val someCards = Set(aceOfSpades,tenOfSpades)

    val myHand = Hand(someCards)
    val myHandWithReverseOrdering = Hand(someCards, ordering = DefaultHandOrdering.reverse)
    val sortedDefault = myHand.getCards
    val sortedReverse = myHandWithReverseOrdering.getCards

    assertResult(aceOfSpades)(sortedDefault.getFirst())
    assertResult(tenOfSpades)(sortedReverse.getFirst())
  }

  "A Hand" should "override toString" in {
    val myHand = Hand(cardsInHand)
    assertResult(validHandString)(myHand.toString())
  }
  "A Hand" should "addCard" in {
    val myHand = Hand(Set(aceOfSpades))
    val newHand = myHand.addCard(tenOfSpades)
    val expectedHand = Hand(Set(tenOfSpades,aceOfSpades))
    assertResult(expectedHand)(newHand)
  }
  "A Hand" should "playCard" in {
    val myHand = Hand(Set(aceOfSpades,tenOfSpades))
    val newHand = myHand.playCard(aceOfSpades)
    val expectedHand = Hand(Set(aceOfSpades,tenOfSpades), Seq(aceOfSpades))
    assertResult(expectedHand)(newHand)
  }
  "A Hand" should "unplayCard" in {
    val myHand = Hand(Set(aceOfSpades,tenOfSpades)).playCard(aceOfSpades)
    val newHand = myHand.unplayCard(aceOfSpades)
    val expectedHand = Hand(Set(aceOfSpades,tenOfSpades))
    assertResult(expectedHand)(newHand)
  }
  "A Hand removeOneRandomCard" should "not remove a played card" in {
    val myHand = Hand(Set(aceOfSpades,tenOfSpades)).playCard(aceOfSpades)
    val newHand = myHand.removeOneRandomCard
    val expectedHand = Hand(Set(aceOfSpades), Seq(aceOfSpades))
    assertResult(expectedHand)(newHand)
  }

  "A Hand size" should "count only unplayed cards" in {
    val myHand = Hand(Set(aceOfSpades,tenOfSpades)).playCard(aceOfSpades)
    assertResult(1)(myHand.size)
  }

  "A Hand" should "return if it contains a card without counting played cards" in {
    val myHand = Hand(Set(aceOfSpades,tenOfSpades)).playCard(aceOfSpades)
    assertResult(false)(myHand.containsCard(aceOfSpades))
    assertResult(true)(myHand.containsCard(tenOfSpades))
    assertResult(false)(myHand.containsCard(kingOfClubs))
  }

  "A Hand" should "return if it has a suit without counting played cards" in {
    val myHand = Hand(Set(aceOfSpades,kingOfClubs)).playCard(aceOfSpades)
    assertResult(false)(myHand.hasSuit(Suit.SPADES))
    assertResult(true)(myHand.hasSuit(Suit.CLUBS))
    assertResult(false)(myHand.hasSuit(Suit.HEARTS))
  }
}

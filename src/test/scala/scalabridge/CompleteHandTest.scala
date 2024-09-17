package scalabridge

import org.scalatest.flatspec.AnyFlatSpec
import scalabridge.CompleteHand
import org.junit.jupiter.api.Test
import java.lang.IllegalArgumentException

@Test
class CompleteHandTest extends AnyFlatSpec {
    val completeCards = "A6.KT2.K85.Q9742"
    val lessCards = "86.KT2.K85.Q974"
    val moreCards = "86.KT2.K85.Q97432"
    val completeCardsWrongChars = "8F.KT2.K8U.Q9742"
    val completeCardsWrongChars2 = "P6.KT2.K85.Q9742"
    val handWithAceOfSpades = completeCards
    val handWithoutAceOfSpades = "K6.KT2.K85.Q9742"
    val aceOfSpades = Card(Suit.SPADES, Rank.ACE)
    "A CompleteHand" should "be valid when constructed using a pbnString" in {
        assert(CompleteHand(completeCards).getValid().isRight)
    }
    "A CompleteHand" should "not be valid when created with a different number of cards." in {
        assert(CompleteHand(lessCards).getValid().isLeft)
        assert(CompleteHand(moreCards).getValid().isLeft)
    }
    "A CompleteHand" should "not be valid when created with wrong characters for ranks." in {
        assert(CompleteHand(completeCardsWrongChars).getValid().isLeft)
        assert(CompleteHand(completeCardsWrongChars2).getValid().isLeft)
    }
    "A CompleteHand_hasCard" should "return if there is a specific card inside the hand" in {
        assert(CompleteHand(handWithAceOfSpades).hasCard(aceOfSpades))
        assert(!CompleteHand(handWithoutAceOfSpades).hasCard(aceOfSpades))
    }
    "A CompleteHand_cards" should "return the correct set of cards" in {
        val actualSet = CompleteHand(completeCards).cards
        val expectedSet = Set(
            Card(Suit.SPADES, Rank.ACE),
            Card(Suit.SPADES, Rank.SIX),
            Card(Suit.HEARTS, Rank.KING),
            Card(Suit.HEARTS, Rank.TEN),
            Card(Suit.HEARTS, Rank.TWO),
            Card(Suit.DIAMONDS, Rank.KING),
            Card(Suit.DIAMONDS, Rank.EIGHT),
            Card(Suit.DIAMONDS, Rank.FIVE),
            Card(Suit.CLUBS, Rank.QUEEN),
            Card(Suit.CLUBS, Rank.NINE),
            Card(Suit.CLUBS, Rank.SEVEN),
            Card(Suit.CLUBS, Rank.FOUR),
            Card(Suit.CLUBS, Rank.TWO),
        )
        assertResult(expectedSet)(actualSet)
    }

}

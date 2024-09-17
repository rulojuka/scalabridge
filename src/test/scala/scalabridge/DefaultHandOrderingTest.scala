package scalabridge

import org.scalatest.flatspec.AnyFlatSpec
import org.junit.jupiter.api.Test

@Test
class DefaultHandOrderingTest extends AnyFlatSpec {
  val subject = DefaultHandOrdering
  val fourOfClubs = Card(Suit.CLUBS, Rank.FOUR)
  val tenOfClubs = Card(Suit.CLUBS, Rank.TEN)
  val fourOfHearts = Card(Suit.HEARTS, Rank.FOUR)
  val tenOfHearts = Card(Suit.HEARTS, Rank.TEN)
  val fourOfDiamonds = Card(Suit.DIAMONDS, Rank.FOUR)
  "A CardInsideHandOrdering " should "return 0 for any comparation to itself" in {
    assertResult(0)(subject.compare(fourOfClubs, fourOfClubs))
  }
  "A CardInsideHandOrdering " should "put higher ranked ranks to the left" in {
    assert(subject.compare(tenOfClubs, fourOfClubs) < 0)
    assert(subject.compare(fourOfClubs, tenOfClubs) > 0)

    assert(subject.compare(fourOfHearts, tenOfHearts) > 0)
    assert(subject.compare(tenOfHearts, fourOfHearts) < 0)
  }
  "A CardInsideHandOrdering " should "put higher ranked suits to the left, but with diamonds and clubs switched" in {
    assert(subject.compare(fourOfHearts, fourOfClubs) < 0)
    assert(subject.compare(fourOfClubs, fourOfDiamonds) < 0)
  }
}

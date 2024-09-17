package scalabridge

object SuitOrderings {

  private def highestFirstAlternatingColorsValues(suit: Suit): Int =
    suit match
      case Suit.SPADES   => 0
      case Suit.HEARTS   => 1
      case Suit.CLUBS    => 2
      case Suit.DIAMONDS => 3

  private def withTrumpSuitValues(trumpSuit: Suit, suit: Suit): Int = {
    (trumpSuit, suit) match
      case (Suit.CLUBS, Suit.CLUBS)       => 0
      case (Suit.CLUBS, Suit.HEARTS)      => 1
      case (Suit.CLUBS, Suit.SPADES)      => 2
      case (Suit.CLUBS, Suit.DIAMONDS)    => 3
      case (Suit.DIAMONDS, Suit.DIAMONDS) => 0
      case (Suit.DIAMONDS, Suit.SPADES)   => 1
      case (Suit.DIAMONDS, Suit.HEARTS)   => 2
      case (Suit.DIAMONDS, Suit.CLUBS)    => 3
      case (Suit.HEARTS, Suit.HEARTS)     => 0
      case (Suit.HEARTS, Suit.SPADES)     => 1
      case (Suit.HEARTS, Suit.DIAMONDS)   => 2
      case (Suit.HEARTS, Suit.CLUBS)      => 3
      case (Suit.SPADES, Suit.SPADES)     => 0
      case (Suit.SPADES, Suit.HEARTS)     => 1
      case (Suit.SPADES, Suit.CLUBS)      => 2
      case (Suit.SPADES, Suit.DIAMONDS)   => 3
  }

  /**
    * This ordering will sort the suits in this order from left to right:
    * SPADES HEARTS CLUBS DIAMONDS
    */
  def highestFirstAlternatingColors: Ordering[Suit] = (x: Suit, y: Suit) =>
    highestFirstAlternatingColorsValues(x) - highestFirstAlternatingColorsValues(y)

  /**
    * This ordering will sort the suits with the trump suit first, then alternating colors.
    * When it has two options, it will put the highest ranked suit first.
    */
  def withTrumpSuit(trumpSuit: Suit): Ordering[Suit] = (x: Suit, y: Suit) =>
    withTrumpSuitValues(trumpSuit, x) - withTrumpSuitValues(trumpSuit, y)
}

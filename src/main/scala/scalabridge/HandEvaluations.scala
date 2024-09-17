package scalabridge

case class HandEvaluations(hand: Hand) {

  // If these are declared later, java interop breaks.
  private val cardsPerSuit: Map[Suit, Set[Card]] =
    Suit.values.map(suit => (suit -> hand.cards.filter(_.suit == suit))).toMap
  private val numberOfCardsPerSuit: Map[Suit, Int] =
    Suit.values.map(suit => (suit -> cardsPerSuit(suit).size)).toMap
  private def isMajorSuit(suit: Suit) = suit match
    case Suit.HEARTS | Suit.SPADES => true
    case _                         => false
  private def isMinorSuit(suit: Suit) = suit match
    case Suit.DIAMONDS | Suit.CLUBS => true
    case _                          => false
  private def isThreeHigherCards(card: Card) = card.rank match
    case Rank.ACE | Rank.KING | Rank.QUEEN => true
    case _                                 => false
  private def isFiveHigherCards(card: Card) = card.rank match
    case Rank.ACE | Rank.KING | Rank.QUEEN | Rank.JACK | Rank.TEN => true
    case _                                                        => false

  val getHCP: Int = hand.cards.map(_.getPoints).sum
  val getShortestSuitLength: Int = numberOfCardsPerSuit.values.minOption.getOrElse(0)
  val getLongestSuitLength: Int = numberOfCardsPerSuit.values.maxOption.getOrElse(0)
  val getNumberOfDoubletonSuits: Int = numberOfCardsPerSuit.values.count(_ == 2)
  val hasFiveOrMoreCardsInAMajorSuit: Boolean =
    numberOfCardsPerSuit.filter((suit, _) => isMajorSuit(suit)).values.maxOption.getOrElse(0) >= 5
  val hasThreeOrMoreCardsInAMinorSuit: Boolean =
    numberOfCardsPerSuit.filter((suit, _) => isMinorSuit(suit)).values.maxOption.getOrElse(0) >= 3
  val isBalanced: Boolean =
    getShortestSuitLength >= 2 && getLongestSuitLength <= 5 && getNumberOfDoubletonSuits <= 1
  val hasEightOrMoreCardsInAnySuit: Boolean = getLongestSuitLength >= 8
  val hasSixCardsInLongestSuit: Boolean = getLongestSuitLength == 6
  val hasSevenCardsInLongestSuit: Boolean = getLongestSuitLength == 7
  def hasTwoOutOfThreeHigherCards(suit: Suit): Boolean =
    cardsPerSuit(suit).count(isThreeHigherCards(_)) >= 2
  def hasThreeOutOfFiveHigherCards(suit: Suit): Boolean =
    cardsPerSuit(suit).count(isFiveHigherCards(_)) >= 3
  val getLongestSuit: Suit = cardsPerSuit.max(ord = (x, y) => x._2.size - y._2.size)._1
  val hasFourOrMoreCardsInMajorSuitExcludingLongestSuit: Boolean = cardsPerSuit
    .filterNot((suit, _) => suit == getLongestSuit)
    .filter((suit, _) => isMajorSuit(suit))
    .exists((suit, cards) => cards.size >= 4)

  /**
    * @return Return the longest major if they have different lengths. If they have equal lengths, return spades.
    */
  def getLongestMajor(): Suit = {
    if (cardsPerSuit(Suit.SPADES).size >= cardsPerSuit(Suit.HEARTS).size)
      Suit.SPADES
    else Suit.HEARTS
  }

  /**
    * @return Return the longest minor if they have different lengths. If they have equal lengths, return clubs.
    */
  def getLongestMinor(): Suit = {
    if (cardsPerSuit(Suit.DIAMONDS).size > cardsPerSuit(Suit.CLUBS).size)
      Suit.DIAMONDS
    else Suit.CLUBS
  }

}

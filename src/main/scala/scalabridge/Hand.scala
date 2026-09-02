package scalabridge

import scalabridge.pbn.PBNUtils

import scala.jdk.javaapi.CollectionConverters
import scala.util.Random

case class Hand(
    allCards: Set[Card],
    playedCards: Seq[Card] = Seq.empty,
    ordering: Ordering[Card] = DefaultHandOrdering
) {
  def this(javaListOfCards: java.util.List[Card]) =
    this(CollectionConverters.asScala(javaListOfCards).toSet)

  private val playedCardsSet: Set[Card] = this.playedCards.toSet
  private val unplayedCardsSet: Set[Card] = this.allCards -- this.playedCardsSet
  private val unplayedCards: Seq[Card] = unplayedCardsSet.toSeq.sorted(using ordering)
  val cards: Set[Card] = unplayedCardsSet
  val getCards: java.util.List[Card] = CollectionConverters.asJava(unplayedCards)
  val isComplete: Boolean = this.allCards.size == GameConstants.SIZE_OF_HAND

  def getHandEvaluations = HandEvaluations(this)
  def addCard(card: Card): Hand = this.copy(allCards = allCards + card)
  def playCard(card: Card): Hand =
    if (unplayedCardsSet.contains(card))
      this.copy(playedCards = card +: playedCards)
    else
      this

  def unplayCard(card: Card): Hand =
    if (playedCardsSet.contains(card))
      this.copy(playedCards = this.playedCards.filterNot(playedCard => playedCard == card))
    else
      this

  def removeOneRandomCard: Hand = {
    val cardToRemove = Random().shuffle(unplayedCards).head
    this.copy(allCards = allCards - cardToRemove)
  }

  val size: Int = unplayedCardsSet.size

  def containsCard(card: Card): Boolean = unplayedCardsSet.contains(card)

  def hasSuit(suit: Suit): Boolean = unplayedCards.exists(card => card.suit == suit)

  def withDefaultOrdering: Hand = this.copy(ordering = DefaultHandOrdering)

  override def toString(): String = PBNUtils.partialDealTagFromHand(this)

}
object Hand {
  def apply(javaListOfCards: java.util.List[Card]) = new Hand(javaListOfCards)
}

package scalabridge

import scala.util.Failure
import scalabridge.exceptions.TrickAlreadyFullException
import scala.util.Success
import scala.util.Try
import scala.jdk.javaapi.CollectionConverters

case class Trick(leader: Direction, cards: Vector[Card]) {
  def this(leader: Direction) =
    this(leader, Vector.empty) // Java interop doesn't work with default value

  private val directionToCardMap: Map[Direction, Card] =
    cards.zipWithIndex.map((card, index) => (leader.next(PositiveInteger(index)) -> card)).toMap
  private val cardToDirectionMap: Map[Card, Direction] =
    cards.zipWithIndex.map((card, index) => (card -> leader.next(PositiveInteger(index)))).toMap
  private val leadSuit: Option[Suit] = if (cards.isEmpty) None else Some(cards(0).suit)

  private def hasSuit(suit: Suit) = cards.map(_.suit).contains(suit);
  private def highestCardOfSuit(suit: Suit): Option[Card] =
    cards
      .filter(_.suit == suit)
      .map(_.rank)
      .sorted(using RankOrderings.highestFirst)
      .headOption
      .map(Card(suit, _))

  val getLeader = this.leader
  val isEmpty: Boolean = cards.isEmpty
  val isComplete = cards.length == GameConstants.COMPLETE_TRICK_NUMBER_OF_CARDS
  val getLeadSuit: Option[Suit] = leadSuit
  val getLastPlayer: Direction = leader.next(PositiveInteger(GameConstants.NUMBER_OF_HANDS - 1))

  def getCardDirectionMap: java.util.Map[Card, Direction] =
    CollectionConverters.asJava(cardToDirectionMap)
  def addCard(card: Card): Try[Trick] =
    if (isComplete)
      Failure(new TrickAlreadyFullException)
    else
      Success(this.copy(cards = cards :+ card))

  /**
    *
    * @param card
    * @return The Trick with the new card added or the same Trick if it is already full.
    */
  def addCardSafe(card: Card): Trick =
    addCard(card) match
      case Failure(exception) => this
      case Success(value)     => value
  def getCards: java.util.List[Card] = CollectionConverters.asJava(cards.toList)
  def getWinnerWithoutTrumpSuit: Option[Direction] =
    if (isComplete)
      assert(leadSuit.isDefined)
      val winnerCard = highestCardOfSuit(leadSuit.get).get
      Some(cardToDirectionMap(winnerCard))
    else None
  def getWinnerWithTrumpSuit(trumpSuit: Suit): Option[Direction] = {
    if (isComplete)
      val winnerCard =
        if (hasSuit(trumpSuit)) highestCardOfSuit(trumpSuit).get
        else
          assert(leadSuit.isDefined)
          highestCardOfSuit(leadSuit.get).get
      Some(cardToDirectionMap(winnerCard))
    else None
  }
  def hasCardOf(direction: Direction) = directionToCardMap.get(direction).isDefined
  def getCardsFromLastUpTo(direction: Direction): java.util.Map[Card, Direction] =
    throw NotImplementedError("This is used for undo, which will work different in scalabridge.")
  def removeCardsFromLastUpTo(direction: Direction): Unit =
    throw NotImplementedError("This is used for undo, which will work different in scalabridge.")

}
object Trick {
  def apply(leader: Direction) = new Trick(leader)
}

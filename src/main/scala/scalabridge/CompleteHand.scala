package scalabridge

import scalabridge.exceptions.InvalidCompleteHandException

case class CompleteHand(hand: Hand) extends Validated[CompleteHand]:
  override def getValid(): Either[Iterable[Throwable], CompleteHand] =
    if (hand.size == GameConstants.SIZE_OF_HAND) Right(this)
    else Left(List(InvalidCompleteHandException(this.hand.toString())))
  val cards: Set[Card] = this.hand.allCards
  def containsCard(card: Card): Boolean = this.hand.containsCard(card)
  override def toString: String = this.hand.toString()

end CompleteHand

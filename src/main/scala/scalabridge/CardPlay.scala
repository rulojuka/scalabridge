package scalabridge

import scalabridge.exceptions.cardplay.CardPlayedTwiceInCardPlayException
import scalabridge.exceptions.cardplay.CardPlayedInAnotherPlayersTurnException

/**
  * The rules for Bridge CardPlay are as follows (paraphrasing/quoting the 2017 Laws of Bridge - LAWS 44-67):
  * 
  *
  */
case class CardPlay(
    hands: CompleteDeckInFourHands,
    tricks: Vector[Trick] = Vector.empty[Trick]
) extends Validated[CardPlay] {

  private val playedCardsSet: Set[Card] = tricks.map(_.cards).flatten.toSet

  /**
    * 
    * Checks recursively if
    * 1. It is valid until one card before
    * 2. It continues valid adding the last card.
    * 
    * For the last card to be valid it must maintain the following invariants:
    * 1. The current card has not been played before in the card play.
    * 2. The current card was originally on the current player's hand.
    * 3. The current card follows suit if possible.
    *
    */
  override def getValid(): Either[Iterable[Throwable], CardPlay] = {
    if (tricks.isEmpty || playedCardsSet.isEmpty) {
      Right(this)
    } else {
      val (currentTrick, previousTricks) =
        if (tricks.last.isEmpty) {
          (tricks.dropRight(1).last, tricks.dropRight(2))
        } else {
          (tricks.last, tricks.dropRight(1))
        }
      assert(!currentTrick.cards.isEmpty)
      val updatedCurrentTrick = currentTrick.copy(cards = currentTrick.cards.dropRight(1))
      val cardPlayToValidate = CardPlay(hands, previousTricks :+ updatedCurrentTrick)
      val validation = cardPlayToValidate.getValid()
      validation match
        case Left(iterable) => Left(iterable)
        case Right(value) => {
          val lastCardPlayed = currentTrick.cards.last
          val cardsPlayed = currentTrick.cards.size
          val currentDirection = currentTrick.leader.next(PositiveInteger(cardsPlayed - 1))
          val cardPlayedTwiceValidation =
            if (cardPlayToValidate.playedCardsSet.contains(lastCardPlayed)) {
              Left(List(new CardPlayedTwiceInCardPlayException))
            } else {
              Right(this)
            }
          val cardPlayedInAnotherTurnValidation =
            if (!hands.getHandOf(currentDirection).cards.contains(lastCardPlayed)) {
              Left(List(new CardPlayedInAnotherPlayersTurnException))
            } else {
              Right(this)
            }
          val cardFollowsSuitValidation = Right(this) // Missing follow suit validation
          val allValidations: List[Either[Iterable[Throwable], CardPlay]] = List(
            cardPlayedTwiceValidation,
            cardPlayedInAnotherTurnValidation,
            cardFollowsSuitValidation
          )
          val failures = allValidations.collect { case Left(throwable) => throwable }
          if (failures.isEmpty) {
            Right(this)
          } else {
            Left(failures.flatten)
          }
        }
    }
  }
}

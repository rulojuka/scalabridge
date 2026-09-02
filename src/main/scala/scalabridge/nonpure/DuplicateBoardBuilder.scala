package scalabridge.nonpure

import scalabridge.CompleteDeckInFourHands
import scalabridge.CompleteHand
import scalabridge.DuplicateBoard
import scalabridge.PositiveInteger
import scalabridge.pbn.PBNDealTag

import scala.util.Failure
import scala.util.Success

object DuplicateBoardBuilder {

  def build(boardNumber: Int, pbnDealTag: String): DuplicateBoard = {
    try {
      val positiveInteger =
        PositiveIntegerValidatedBuilder.build(PositiveInteger(boardNumber))
      val validDealTag = PBNDealTag(pbnDealTag).getValid() match
        case Left(value)  => throw value.head
        case Right(value) => value
      val (firstDirection, handOptions) = validDealTag.getParts match
        case Failure(exception) => throw exception
        case Success(value)     => value
      val completeHands = handOptions.map(handOption => CompleteHand(handOption.get))
      val validatedHands =
        completeHands.map((completeHand) => CompleteHandValidatedBuilder.build(completeHand))
      val directionToCompleteHandMap = validatedHands.zipWithIndex
        .map((hand, index) => (firstDirection.next(PositiveInteger(index)) -> hand))
        .toMap
      val finalHands = CompleteDeckInFourHandsValidatedBuilder.build(
        CompleteDeckInFourHands(directionToCompleteHandMap)
      )
      DuplicateBoard(positiveInteger, finalHands)
    } catch {
      case illegalArgumentException: IllegalArgumentException =>
        throw illegalArgumentException
      case exception: Exception =>
        throw new IllegalArgumentException(exception.getMessage())
    }
  }

}

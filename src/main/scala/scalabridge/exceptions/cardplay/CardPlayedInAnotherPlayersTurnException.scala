package scalabridge.exceptions.cardplay

final class CardPlayedInAnotherPlayersTurnException
    extends IllegalArgumentException("You cannot play a card in another player's turn.")

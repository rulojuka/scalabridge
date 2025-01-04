package scalabridge.exceptions.cardplay

final class CardPlayedTwiceInCardPlayException
    extends IllegalStateException("You cannot play the same card twice during a card play.")

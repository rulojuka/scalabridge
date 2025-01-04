package scalabridge.exceptions.cardplay

final class CardPlayWithIncompleteTrickBeforeCurrentTrickException
    extends IllegalStateException("There cannot be incomplete tricks before the current trick.")

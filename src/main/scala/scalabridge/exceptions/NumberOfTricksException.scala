package scalabridge.exceptions

import scalabridge.GameConstants

final case class NumberOfTricksException(val illegalNumberOfTricks: String)
    extends IllegalArgumentException(
      s"The number of tricks ${illegalNumberOfTricks} is invalid. Number of tricks should be between 0 and ${GameConstants.SIZE_OF_HAND} inclusive."
    )

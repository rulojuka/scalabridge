package scalabridge

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scalabridge.exceptions.NumberOfTricksException

enum NumberOfTricks(val tricks: Int) {
  case ZERO extends NumberOfTricks(0)
  case ONE extends NumberOfTricks(1)
  case TWO extends NumberOfTricks(2)
  case THREE extends NumberOfTricks(3)
  case FOUR extends NumberOfTricks(4)
  case FIVE extends NumberOfTricks(5)
  case SIX extends NumberOfTricks(6)
  case SEVEN extends NumberOfTricks(7)
  case EIGHT extends NumberOfTricks(8)
  case NINE extends NumberOfTricks(9)
  case TEN extends NumberOfTricks(10)
  case ELEVEN extends NumberOfTricks(11)
  case TWELVE extends NumberOfTricks(12)
  case THIRTEEN extends NumberOfTricks(13)
}
object NumberOfTricks {
  private val mapFromInt: Map[Int, NumberOfTricks] =
    NumberOfTricks.values.map(numberOfTricks => numberOfTricks.tricks -> numberOfTricks).toMap

  def fromInt(number: Int): Try[NumberOfTricks] = {
    NumberOfTricks.mapFromInt.get(number) match {
      case Some(numberOfTricks) => Success(numberOfTricks)
      case None                 => Failure(NumberOfTricksException(number.toString()))
    }
  }
}

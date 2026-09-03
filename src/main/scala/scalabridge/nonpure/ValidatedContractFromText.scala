package scalabridge.nonpure

import scalabridge.Contract
import scalabridge.OddTricks
import scalabridge.Strain
import scalabridge.PenaltyStatus
import scalabridge.Validated
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scalabridge.exceptions.OddTricksException
import scalabridge.AllPassContract

/**
   * @param text should be in the format ("ALLPASS")|(LS[P]) where
   * "ALLPASS" is this exact string
   * L = Odd Tricks level (a digit from 1 to 7)
   * S = strain ( [c,d,h,s,n] )
   * P = Penalty ( "X" for double and "XX" for redouble )
   */
case class ValidatedContractFromText(text: String) extends Validated[Contract] {

  override def getValid(): Either[Iterable[Throwable], Contract] = {
    text match
      case "ALLPASS" => Right(AllPassContract)
      case _         => {
        val oddTricksTry = text.substring(0, 1).toIntOption match
          case Some(level) => OddTricks.fromLevel(level)
          case None        => Failure(OddTricksException(text))

        val strainText: String = text.substring(1, 2).toUpperCase
        val strainTry: Try[Strain] = strainFromSymbol(strainText)

        val penaltyText = text.length() match
          case 0 | 1 | 2 => ""
          case _         => text.substring(2)
        val penaltyStatusTry: Try[PenaltyStatus] = penaltyText match
          case ""   => Success(PenaltyStatus.NONE)
          case "X"  => Success(PenaltyStatus.DOUBLED)
          case "XX" => Success(PenaltyStatus.REDOUBLED)
          case _    => Failure(IllegalArgumentException()) // FIXME create specific exception

        val throwables = Seq(oddTricksTry, strainTry, penaltyStatusTry).partitionMap(_.toEither)._1
        if (throwables.isEmpty)
          Right(Contract(oddTricksTry.get, strainTry.get, penaltyStatusTry.get))
        else
          Left(throwables)
      }
  }

  private def strainFromSymbol(symbol: String): Try[Strain] = // FIXME move this into Strain
    symbol match
      case "C" => Success(Strain.CLUBS)
      case "D" => Success(Strain.DIAMONDS)
      case "H" => Success(Strain.HEARTS)
      case "S" => Success(Strain.SPADES)
      case "N" => Success(Strain.NOTRUMPS)
      case _   => Failure(IllegalArgumentException())

}

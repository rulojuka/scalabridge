package scalabridge

object RankOrderings {

  private def highestFirstValues(rank: Rank): Int =
    rank match
      case Rank.ACE   => 0
      case Rank.KING  => 1
      case Rank.QUEEN => 2
      case Rank.JACK  => 3
      case Rank.TEN   => 4
      case Rank.NINE  => 5
      case Rank.EIGHT => 6
      case Rank.SEVEN => 7
      case Rank.SIX   => 8
      case Rank.FIVE  => 9
      case Rank.FOUR  => 10
      case Rank.THREE => 11
      case Rank.TWO   => 12

  /**
    * This ordering will sort the ranks in this order from left to right: 
    * AKQJT98765432
    */
  val highestFirst: Ordering[Rank] = (x: Rank, y: Rank) => highestFirstValues(x) - highestFirstValues(y)
}

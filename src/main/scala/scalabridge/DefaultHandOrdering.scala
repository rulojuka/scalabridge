package scalabridge

import scala.collection.immutable.HashMap

object DefaultHandOrdering extends Ordering[Card] {

  override def compare(x: Card, y: Card): Int =
    SuitOrderings.highestFirstAlternatingColors.compare(x.suit, y.suit) match
      case 0       => RankOrderings.highestFirst.compare(x.rank, y.rank)
      case nonzero => nonzero

}

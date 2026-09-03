package scalabridge

import org.junit.jupiter.api.Test
import _root_.scalabridge.nonpure.ContractFromTextValidatedBuilder

@Test
class ContractTest extends UnitFunSpec {
  private def getContract(text: String) =
    ContractFromTextValidatedBuilder.build(text)
  describe("A Contract") {
    it("should be constructable from text") {
      val allPass = AllPassContract
      val fourSpades =
        Contract(OddTricks.FOUR, Strain.SPADES, PenaltyStatus.NONE)
      val fourSpadesDoubled =
        Contract(
          OddTricks.FOUR,
          Strain.SPADES,
          PenaltyStatus.DOUBLED
        )
      val fourSpadesRedoubled =
        Contract(
          OddTricks.FOUR,
          Strain.SPADES,
          PenaltyStatus.REDOUBLED
        )
      getContract("ALLPASS") shouldBe allPass
      getContract("4S") shouldBe fourSpades
      getContract("4SX") shouldBe fourSpadesDoubled
      getContract("4SXX") shouldBe fourSpadesRedoubled
    }
    it("should have an ALLPASS option") {
      val subject = AllPassContract
      subject.isAllPass shouldBe true
      subject.toString() shouldBe "ALLPASS"
    }
  }
}

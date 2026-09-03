package scalabridge

import org.junit.jupiter.api.Test
import _root_.scalabridge.nonpure.ContractFromTextValidatedBuilder

@Test
class ScoreTest extends UnitFunSpec {
  private def getScore(
      text: String,
      vulnerabilityStatus: VulnerabilityStatus,
      tricksMade: NumberOfTricks
  ): Int = {
    val contract = ContractFromTextValidatedBuilder.build(text)
    val score = Score(contract, vulnerabilityStatus, tricksMade)
    score.calculate
  }

  private val nonVul = VulnerabilityStatus.NONVULNERABLE
  private val vul = VulnerabilityStatus.VULNERABLE
  describe("A ScoreCalculator") {
    it("should score 1N= correctly") {
      getScore("1N", nonVul, NumberOfTricks.SEVEN) shouldBe 90
      getScore("1N", vul, NumberOfTricks.SEVEN) shouldBe 90
    }
    it("should score 3N= correctly") {
      getScore("3N", nonVul, NumberOfTricks.NINE) shouldBe 400
      getScore("3N", vul, NumberOfTricks.NINE) shouldBe 600
    }
    it("should score 7NXX= correctly") {
      getScore("7NXX", nonVul, NumberOfTricks.THIRTEEN) shouldBe 2280
      getScore("7NXX", vul, NumberOfTricks.THIRTEEN) shouldBe 2980
    }
    it("should score some arbitrary contracts correctly") {
      getScore("1C", vul, NumberOfTricks.SEVEN) shouldBe 70;
      getScore("1H", vul, NumberOfTricks.SEVEN) shouldBe 80;
      getScore("1C", vul, NumberOfTricks.EIGHT) shouldBe 90;
      getScore("1N", vul, NumberOfTricks.SEVEN) shouldBe 90;
      getScore("1H", vul, NumberOfTricks.EIGHT) shouldBe 110;
      getScore("1N", vul, NumberOfTricks.EIGHT) shouldBe 120;
      getScore("2DX", vul, NumberOfTricks.EIGHT) shouldBe 180;
      getScore("1NX", vul, NumberOfTricks.SEVEN) shouldBe 180;
      getScore("3S", nonVul, NumberOfTricks.TWELVE) shouldBe 230;
      getScore("2DX", vul, NumberOfTricks.NINE) shouldBe 380;
      getScore("5D", nonVul, NumberOfTricks.TWELVE) shouldBe 420;
      getScore("5D", vul, NumberOfTricks.TWELVE) shouldBe 620;
      getScore("3DX", vul, NumberOfTricks.NINE) shouldBe 670;
      getScore("3NX", nonVul, NumberOfTricks.ELEVEN) shouldBe 750;
      getScore("2DXX", vul, NumberOfTricks.EIGHT) shouldBe 760;
      getScore("3DX", vul, NumberOfTricks.TEN) shouldBe 870;
      getScore("6H", nonVul, NumberOfTricks.TWELVE) shouldBe 980;
      getScore("2DXX", vul, NumberOfTricks.NINE) shouldBe 1160;
      getScore("3HX", vul, NumberOfTricks.TWELVE) shouldBe 1330;
      getScore("6C", vul, NumberOfTricks.TWELVE) shouldBe 1370;
      getScore("6H", vul, NumberOfTricks.TWELVE) shouldBe 1430;
    }
    it("should score all failed contracts correctly") {
      // Trusting completely in http://rpbridge.net/cgi-bin/xsc2.pl
      val values = Seq(
        (1, -50, -100, -200, -100, -200, -400), // -1
        (2, -100, -300, -600, -200, -500, -1000), // -2
        (3, -150, -500, -1000, -300, -800, -1600), // -3
        (4, -200, -800, -1600, -400, -1100, -2200), // -4
        (5, -250, -1100, -2200, -500, -1400, -2800), // -5
        (6, -300, -1400, -2800, -600, -1700, -3400), // -6
        (7, -350, -1700, -3400, -700, -2000, -4000), // -7
        (8, -400, -2000, -4000, -800, -2300, -4600), // -8
        (9, -450, -2300, -4600, -900, -2600, -5200), // -9
        (10, -500, -2600, -5200, -1000, -2900, -5800), // -10
        (11, -550, -2900, -5800, -1100, -3200, -6400), // -11
        (12, -600, -3200, -6400, -1200, -3500, -7000), // -12
        (13, -650, -3500, -7000, -1300, -3800, -7600) // -13
      )
      val sevenOddTricks = OddTricks.SEVEN
      val anyStrain = Strain.NOTRUMPS
      val none = PenaltyStatus.NONE
      val doubled = PenaltyStatus.DOUBLED
      val redoubled = PenaltyStatus.REDOUBLED
      val nonvulnerable = VulnerabilityStatus.NONVULNERABLE
      val vulnerable = VulnerabilityStatus.VULNERABLE

      val nonVul = Contract(sevenOddTricks, anyStrain, none);
      val nonVulX = Contract(sevenOddTricks, anyStrain, doubled)
      val nonVulXX = Contract(sevenOddTricks, anyStrain, redoubled)
      val vul = Contract(sevenOddTricks, anyStrain, none)
      val vulX = Contract(sevenOddTricks, anyStrain, doubled)
      val vulXX = Contract(sevenOddTricks, anyStrain, redoubled)
      values.foreach(downSome =>
        val (i, a, b, c, d, e, f) = downSome
        val tricksMade = NumberOfTricks.fromInt(13 - i).get
        Score(nonVul, nonvulnerable, tricksMade).calculate shouldBe a
        Score(nonVulX, nonvulnerable, tricksMade).calculate shouldBe b
        Score(nonVulXX, nonvulnerable, tricksMade).calculate shouldBe c
        Score(vul, vulnerable, tricksMade).calculate shouldBe d
        Score(vulX, vulnerable, tricksMade).calculate shouldBe e
        Score(vulXX, vulnerable, tricksMade).calculate shouldBe f
      )
    }
  }
}

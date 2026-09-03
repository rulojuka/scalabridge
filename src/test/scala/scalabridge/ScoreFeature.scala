package scalabridge

import scala.io.Source
import _root_.scalabridge.nonpure.ContractFromTextValidatedBuilder

class ScoreFeature extends FeatureSpec {

  info("As a bridge core server")
  info("I want to be able to run thousands of valid scores")
  info("So players can use this feature")
  info("And be sure it is always working correctly")

  Feature("Score") {
    Scenario("User creates thousands of valid scores") {
      Given("a file with thousands of scores, one per line")
      val resource = Source.fromResource("scores-valid.txt") // Side-effect
      val lines: Iterator[String] = resource.getLines()

      When("each Score and expected value is created with the data")
      val allScoresAndValues = lines.map(createScoreAndValueFromString(_))

      Then("the creation should not throw errors and every score should be correct")
      allScoresAndValues.foreach((score, expectedValue) => { // Side-effect
        score.value shouldBe expectedValue
      })
    }

    def createScoreAndValueFromString(line: String): (Score, Int) = {
      val listOfStrings = line.split(" ").toList
      assert(listOfStrings.size == 4, "The input file should be in this format")
      val stringsTuple = (listOfStrings(0), listOfStrings(1), listOfStrings(2), listOfStrings(3))
      (
        Score(
          ContractFromTextValidatedBuilder.build(stringsTuple._1),
          stringsTuple._2 match {
            case "VUL"  => VulnerabilityStatus.VULNERABLE
            case "NVUL" => VulnerabilityStatus.NONVULNERABLE
            case _      => throw new IllegalArgumentException
          },
          NumberOfTricks.fromInt(stringsTuple._3.toInt).get
        ),
        stringsTuple._4.toInt
      )
    }
  }
}

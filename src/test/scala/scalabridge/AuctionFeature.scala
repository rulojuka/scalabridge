package scalabridge

import scala.io.Source

class AuctionFeature extends FeatureSpec {

  info("As a bridge core server")
  info("I want to be able to run thousands of valid auctions")
  info("So players can use this feature")
  info("And be sure it is always working correctly")

  Feature("Auction") {
    Scenario("User creates thousands of valid auctions") {
      Given("a file with thousands of auctions, one per line")
      val resource = Source.fromResource("auctions-valid.txt") // Side-effect
      val lines: Iterator[String] = resource.getLines()

      When("each Auction is created with the data")
      val allAuctions = lines.map(createAuctionFromString(_))

      Then("the creation should not throw errors and each auction should be finished and valid")
      allAuctions.foreach((currentAuction) => { // Side-effect
        currentAuction.isFinished shouldBe true
        currentAuction.getValid() shouldBe Right(currentAuction)
      })

    }

    def createAuctionFromString(line: String): Auction = {
      val calls = line.split(" ").map(call => BiddingBox.getOption(call).get).toList
      val anyDirection = Direction.EAST
      Auction(anyDirection, calls.reverse)
    }
  }
}

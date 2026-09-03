package scalabridge

package scalabridge

import org.junit.jupiter.api.Test
import scala.util.Success
import _root_.scalabridge.exceptions.NumberOfTricksException

@Test
class NumberOfTricksTest extends UnitFunSpec {
  describe("An NumberOfTricks") {
    it("should be constructable from Int") {
      NumberOfTricks.fromInt(0) shouldBe Success(NumberOfTricks.ZERO)
      NumberOfTricks.fromInt(3) shouldBe Success(NumberOfTricks.THREE)
      NumberOfTricks.fromInt(13) shouldBe Success(NumberOfTricks.THIRTEEN)
    }
    it("should return the correct exception when fromInt fails") {
      val illegalNumber = -1
      val anotherIllegalNumber = 14
      val exception: NumberOfTricksException =
        NumberOfTricks.fromInt(illegalNumber).failed.get.asInstanceOf[NumberOfTricksException]
      exception.illegalNumberOfTricks shouldBe illegalNumber.toString
      val anotherException: NumberOfTricksException =
        NumberOfTricks
          .fromInt(anotherIllegalNumber)
          .failed
          .get
          .asInstanceOf[NumberOfTricksException]
      anotherException.illegalNumberOfTricks shouldBe anotherIllegalNumber.toString
    }
  }

}

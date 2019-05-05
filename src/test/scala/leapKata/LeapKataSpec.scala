package leapKata

import org.scalatest.{Matchers, WordSpec}

class LeapKataSpec extends WordSpec with Matchers {

  import YearOps._

  "A year" when {

    "is leap" should {

      "be divisible for 4" in {
        checkLeap(1996)
        checkNotLeap(1997)
      }

      "be divisible for 400" in {
        checkLeap(1600)
        checkNotLeap(1800)
      }

    }

  }

  private def checkLeap(year: Int) =
    isLeap(year) shouldBe true

  private def checkNotLeap(year: Int) =
    isLeap(year) shouldBe false
}

package chainNumbers

import org.scalatest.{Matchers, WordSpec}

class ChainNumbersSpec extends WordSpec with Matchers {

  import ChainNumbers._

  "ChainNumber" should {

    "be able to desc a number" in {

      desc(1234) shouldBe 4321
      desc(1230) shouldBe 3210

    }

    "be able to asc a number" in {

      asc(1234) shouldBe 1234
      asc(324) shouldBe 234

    }

    "find a chain length" in {

      chain(5) shouldBe 2
      chain(1234) shouldBe 4
      chain(123456789) shouldBe 2
      chain(444) shouldBe 2
      chain(34543) shouldBe 8

    }

  }

}

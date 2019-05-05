package chainNumbers

import org.scalatest.{Matchers, WordSpec}

class ChainNumbersSpec extends WordSpec with Matchers {

  import ChainNumbers._

  "ChainNumber" should {

    "find a chain length" in {

      chain(5) shouldBe 2
      chain(1234) shouldBe 4
      chain(123456789) shouldBe 2
      chain(444) shouldBe 2
      chain(34543) shouldBe 8

    }

  }

}

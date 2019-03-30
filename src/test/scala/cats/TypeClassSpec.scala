package cats

import org.scalatest.{Matchers, WordSpec}
import PrintableInstance._

class TypeClassSpec extends WordSpec with Matchers {

  "Printable" should {
    val cat = Cat("name", 3, "blu")

    "work as type class" in {

      Printable.format(cat) shouldBe "name is a 3 old blu cat"

    }

    "wrok with syntax" in {

      import PrintableSyntax._

      cat.format shouldBe "name is a 3 old blu cat"

    }

  }

}

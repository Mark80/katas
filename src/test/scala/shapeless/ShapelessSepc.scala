package shapeless

import org.scalatest.{Matchers, WordSpec}
import shapeless._
import shapeless.ops.hlist._
import shapeless.Generic
import shapeless.Generic.Aux

class ShapelessSpec extends WordSpec with Matchers {

  "Generic" should {

    "works" in {

      val prova = Prova("a", 1)

      val gen: Aux[Prova, ::[String, ::[Long, HNil]]] = Generic[Prova]

      import syntax.std.traversable._
      val x = List("a", "b")
      val xHList = x.toHList[String :: String :: HNil]
      val t: (String, String) = xHList.get.tupled

      //val result: Prova = Prova.tupled(t)

      val x2 = List(1, 2, 3)
      val y = x2.toHList[Int :: Int :: Int :: HNil]
      val z = y.get.tupled

      z shouldBe (1, 2, 3)

    }

  }

}

case class Prova(a: String, b: Long)

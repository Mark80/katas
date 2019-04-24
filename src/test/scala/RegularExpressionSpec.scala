import org.scalatest.{Matchers, WordSpec}

import scala.util.matching.Regex

class RegularExpressionSpec extends WordSpec with Matchers{


  "Regex" should {

    "match string" in {

      val regex: Regex = "[a-z]+\\.txt".r
      val iterator = regex.findAllMatchIn("mmm.txt,ppp.txt")
      iterator.toList.map(_.toString()) shouldBe List("mmm.txt","ppp.txt")

    }

    "match host" ignore {

      val regex = "www\\.(.+).+".r
      val iterator = regex.findAllIn("www.google.it")
      iterator.toList.map(_.toString()) shouldBe List("google")

    }

    "match starting of line" in {

      val regex = "^(From|To):".r
      val iterator = regex.findAllIn("From: Marco,\npippo From: Marco").toList.map(_.toString)
      iterator shouldBe List("From:")

    }

  }

}

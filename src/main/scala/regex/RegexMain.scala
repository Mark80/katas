package regex

import scala.util.matching.Regex

object RegexMain {

  def main(args: Array[String]): Unit = {

    val capturing: Regex = "(?<login>\\w+) (?<id>\\d+)".r

    println(capturing.findAllIn("TEST 123").group("id"))

  }

}

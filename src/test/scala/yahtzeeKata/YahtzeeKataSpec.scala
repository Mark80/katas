package yahtzeeKata

import org.scalatest.{Matchers, WordSpec}
import yahtzeeKata.Category._
import yahtzeeKata.Yahtzee.score

class YahtzeeKataSpec extends WordSpec with Matchers {

  "Yahtzee" should {

    "Chance category is the sum of the value" in {

      val rolls = Rolls(List(1, 1, 1, 1, 1))
      score(rolls, Chance) shouldBe 5
    }

    "Small straight" in {
      val rolls = Rolls(List(1, 2, 3, 4, 5))
      val rolls2 = Rolls(List(1, 2, 3, 5, 5))

      score(rolls, SmallStraight) shouldBe 15
      score(rolls2, SmallStraight) shouldBe 0

    }
  }

}

object Yahtzee {

  def score(rolls: Rolls, category: Category): Int = category match {
    case Chance => rolls.dices.sum
    case SmallStraight =>
      if (rolls.dices == List(1, 2, 3, 4, 5))
        rolls.dices.sum
      else
        0
  }

}

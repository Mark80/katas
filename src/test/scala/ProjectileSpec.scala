import org.scalatest.{Matchers, WordSpec}

import scala.math.BigDecimal.RoundingMode

class ProjectileSpec extends WordSpec with Matchers {

  "Projectile" should {
    "write correct equation for y" in {

      getHeightEquation(startingHeight = 5, startingVelocity = 5, angle = 0) shouldBe "-16.0t^2 + 0.0t + 5.0"
      getHeightEquation(startingHeight = 5, startingVelocity = 2, angle = 45) shouldBe "-16.0t^2 + 1.414t + 5.0"

    }

    "write correct equation for x" in {

      getHeightEquation(startingHeight = 5, startingVelocity = 5, angle = 0) shouldBe "-16.0t^2 + 0.0t + 5.0"
      getHeightEquation(startingHeight = 5, startingVelocity = 2, angle = 45) shouldBe "-16.0t^2 + 1.414t + 5.0"

    }

    "rounding int value" in {

      Projectile.round("2.000") shouldBe "2.0"

    }

  }

  private def getHeightEquation(startingHeight: Double, startingVelocity: Double, angle: Double) =
    new Projectile(startingHeight, startingVelocity, angle).heightEq()
}

class Projectile(startingHeight: Double, startingVelocity: Double, angle: Double) {

  import Math._

  def heightEq(): String = {
    val yVelocity = Projectile.round(BigDecimal.valueOf(startingVelocity * sin(toRadians(angle))).setScale(3, RoundingMode.HALF_EVEN).toString())
    s"-16.0t^2 + ${yVelocity}t + $startingHeight"
  }

}

object Projectile {

  def round(stringNumber: String): String = {
    val Array(integer, decimal) = stringNumber.split("\\.")
    if (decimal == "000")
      integer + ".0"
    else
      stringNumber

  }

}

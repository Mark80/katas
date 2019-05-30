package tagless

import cats.Id
import cats.data.EitherT
import org.scalatest.{Matchers, WordSpec}

class TaglessSpec extends WordSpec with Matchers with EitherTValues {

  implicit val repo: RegistrationRepository[Id] = new RegistrationRepository[Id]

  "Tagless" should {

    import RegistrationService._

    "works" in {

      registerAndLogin("email", "password").rightValue shouldBe ("eccomi1", "eccomi2")

    }

  }

}

trait EitherTValues {

  this: WordSpec =>

  implicit class EitherValues[L, A, B](either: EitherT[Id, L, B]) {

    def rightValue: B = either.value.getOrElse(fail)
    def leftValue: L = either.value match {
      case Left(t) => t
      case _       => fail
    }

  }

}

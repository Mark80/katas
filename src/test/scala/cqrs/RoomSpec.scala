package cqrs

import org.scalatest.{BeforeAndAfterEach, EitherValues, Matchers, WordSpec}

class RoomSpec extends WordSpec with Matchers with EitherValues with BeforeAndAfterEach {


  "A User" should {

    "book a room" in {
      val booking = Booking("clientId", "roomName", "arrival date", "departure date")
      val bookingValidation: Booking => Right[String, Booking] = _ => Right(booking)
      BookingRegistry.register(bookingValidation, _ => Right(()))(booking) shouldBe Right(())
    }

    "give back an error if the room doesn't exist" in {
      val booking = Booking("clientId", "not Exist", "arrival date", "departure date")
      BookingRegistry.register(_ => Left("Error"), _ => Right(()))(booking) shouldBe Left("Error")
    }

  }


}

trait BookingLedger {
  def write: Booking => Either[String, Unit]
}

object BookingRegistry {

  def register(bookingValidation: Booking => Either[String, Booking],
               write: Booking => Either[String, Unit]): Booking => Either[String, Unit] =
    for {
      q <- bookingValidation
      r <- write(q)
    } yield r

  implicit class FunctionMonad[T, E, A](f1: T => Either[E, A]) {

    def map[B](f2: A => B): T => Either[E, B] = (t: T) => {
      f1(t).map(f2)
    }

    def flatMap[B](f2: A => Either[E, B]): T => Either[E, B] = (t: T) => {
      f1(t).flatMap(f2)
    }
  }

}


trait BookingChecker {
  def validate: Booking => Either[String, Booking]
}


case class Room(name: String)

case class Booking(clientId: String,
                   roomName: String,
                   arrivalDate: String,
                   departureDate: String)


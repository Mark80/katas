package booking

import cats.Monad
import cats.implicits._

class HotelService[F[_]](implicit monad: Monad[F]) {

  def findHotelBy(hotelId: String)(implicit repo: Repository[F, Hotel]): F[Option[Hotel]] =
    repo.find(hotelId)

  def addHotel(id: String, name: String)(implicit repo: Repository[F, Hotel]): F[Either[Throwable, Unit]] = {

    repo.find(id)

    for {
      mayBeHotel <- repo.find(id)
      _ <- repo.add(Hotel(id, name))
    } yield ???
  }

}

trait Repository[F[_], A] {

  def add(elem: A): F[Unit]
  def find(id: String): F[Option[A]]

}

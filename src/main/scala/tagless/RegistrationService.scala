package tagless

import cats.Monad
import cats.data.EitherT

sealed trait Error
case class RegistrationError(error: String) extends Error
case class AuthError(error: String) extends Error

trait RegistrationAlgebra[F[_]] {

  import ResultType._

  def registration(email: String, password: String): Eff[F, String]
  def auth(email: String, password: String): Eff[F, String]

}

object ResultType {

  type Eff[F[_], R] = EitherT[F, Throwable, R]

  def apply[F[_], R](f: => R)(implicit app: Monad[F]): Eff[F, R] =
    EitherT(app.pure(Right(f)))

}

class RegistrationRepository[F[_]: Monad] extends RegistrationAlgebra[F] {

  import ResultType._

  def registration(email: String, password: String): Eff[F, String] =
    apply[F, String]("eccomi1")

  def auth(email: String, password: String): Eff[F, String] =
    apply[F, String]("eccomi2")

}

object RegistrationService {

  def registerAndLogin[F[_]: Monad](email: String, password: String)(
      implicit registrationAlgebra: RegistrationAlgebra[F]): EitherT[F, Throwable, (String, String)] =
    for {
      r1 <- registrationAlgebra.registration(email, password)
      r2 <- registrationAlgebra.auth(email, password)
    } yield (r1, r2)

}

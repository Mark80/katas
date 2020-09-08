package catstudy

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait MonadM[F[_]] {

  def pure[A](a: A): F[A]
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

}

object UsingMonad {

  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "8080"
  )

  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  trait HttpService[M[_]] {

    def getConnection(config: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]

  }

  def getResponse[M[_]: Monad](httpService: HttpService[M]) =
    for {
      connection <- httpService.getConnection(config)
      response <- httpService.issueRequest(connection, "payload")
    } yield response

  object HttpServiceTry extends HttpService[Try] {

    def getConnection(config: Map[String, String]): Try[Connection] =
      (for {
        host <- config.get("host")
        port <- config.get("port")
      } yield Connection(host, port)) match {
        case None             => Failure(new Exception("hola"))
        case Some(connection) => Success(connection)
      }

    def issueRequest(connection: Connection, payload: String) = ???

  }

  def main(args: Array[String]): Unit = {}
}

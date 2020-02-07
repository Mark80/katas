package httpsfours

import cats.data.Kleisli
import cats.effect._
import cats.implicits._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.{Request, Response}

import scala.concurrent.ExecutionContext.Implicits.global

object HttpMain extends IOApp {

  import Route._

  implicit val cs: ContextShift[IO] = IO.contextShift(global)

  val httpApp: Kleisli[IO, Request[IO], Response[IO]] =
    Router("/api" -> routes).orNotFound

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

}

case class Tweet(id: Int, message: String)

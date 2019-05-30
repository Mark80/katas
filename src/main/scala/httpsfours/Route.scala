package httpsfours

import cats.effect._
import cats.implicits._
import org.http4s.{EntityEncoder, HttpRoutes}
import org.http4s.circe.jsonEncoderOf
import org.http4s.dsl.io._
import io.circe.generic.auto._

object Route extends JsonFormat {

  val service = new ServiceTweet

  val helloWorldService: HttpRoutes[IO] =
    HttpRoutes
      .of[IO] {
        case GET -> Root / "hello" / name =>
          Ok(s"Hello, $name.")
      }

  val getTweet: HttpRoutes[IO] =
    HttpRoutes
      .of[IO] {
        case GET -> Root / "tweet" =>
          Ok(service.getPopularTweets())
      }

  val routes = helloWorldService <+> getTweet

}

trait JsonFormat {

  implicit val encoder: EntityEncoder[IO, Tweet] = jsonEncoderOf[IO, Tweet]
  implicit val encoder2: EntityEncoder[IO, List[Tweet]] = jsonEncoderOf[IO, List[Tweet]]

}

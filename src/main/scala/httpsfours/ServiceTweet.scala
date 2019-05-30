package httpsfours

import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

class ServiceTweet {

  implicit val cs: ContextShift[IO] = IO.contextShift(global)

  def getPopularTweets(): IO[List[Tweet]] = cs.shift *> IO(List(Tweet(1, "message")))

}

package catstudy

import scala.concurrent.duration.FiniteDuration

object TypeSocket {

  def main(args: Array[String]): Unit = {}

}

trait TypeSocket[F[_]] {

  def write(content: Array[Byte], timeout: FiniteDuration): F[Unit]

}

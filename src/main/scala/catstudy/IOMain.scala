package catstudy

import cats.effect.IO

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object IOMain {
  def putStrLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)

  def main(args: Array[String]): Unit = {

    val program: IO[Unit] = for {
      _ <- putStrLn(value = "What's your name?")
      n <- readLn
      _ <- putStrLn(value = s"Hello, $n!")
    } yield ()

    program.unsafeRunSync()

  }

  def convert[A](fut: => Future[A])(implicit executionContext: ExecutionContext): IO[A] =
    IO.async { cb =>
      fut.onComplete {
        case Success(value)     => cb(Right(value))
        case Failure(exception) => cb(Left(exception))

      }

    }

}

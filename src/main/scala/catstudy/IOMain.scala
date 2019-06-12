package catstudy

import java.util.concurrent.Executors

import cats.effect.{ContextShift, Fiber, IO}
import org.http4s.client.Client
import cats.effect.IO
import cats.effect.internals.IOContextShift
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

object IOMain {

  def putStrLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)

  val ops = List()

  def main(args: Array[String]): Unit = {

    val program: IO[Unit] = (for {
      _ <- IO(println(s"What's your name?${Random.nextInt(10)}"))
      p <- readLn
      _ <- putStrLn(value = s"Hello, $p!")
    } yield ()).foreverM

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

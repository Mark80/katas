package catstudy

import java.util.concurrent.{ExecutorService, Executors}

import cats.effect.{IO, IOApp}
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

object IOMain extends IOApp {

  def putStrLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine)

  val cachedThreadPool: ExecutorService = Executors.newCachedThreadPool()
  val BlockingFileIO: ExecutionContext = ExecutionContext.fromExecutor(cachedThreadPool)

  def run(args: List[String]): IO[Nothing] =
    (for {
      _ <- IO(println(s"What's your name?${Random.nextInt(10)}"))
      _ <- IO.shift(BlockingFileIO)
      p <- readLn
      _ <- IO.shift
      _ <- putStrLn(value = s"Hello, $p!")
    } yield ()).foreverM

  def convert[A](fut: => Future[A])(implicit executionContext: ExecutionContext): IO[A] =
    IO.async { cb =>
      fut.onComplete {
        case Success(value)     => cb(Right(value))
        case Failure(exception) => cb(Left(exception))

      }

    }

}

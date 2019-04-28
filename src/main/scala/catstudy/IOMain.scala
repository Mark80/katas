package catstudy

import cats.effect.IO

object IOMain {
  def putStrLn(value: String) = IO(println(value))
  val readLn = IO(scala.io.StdIn.readLine("\n"))

  def main(args: Array[String]): Unit = {

    val program: IO[Unit] = for {
      _ <- putStrLn("What's your name?")
      n <- readLn
      _ <- putStrLn(s"Hello, $n!")
    } yield ()

    program.unsafeRunSync()

  }

}

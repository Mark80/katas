package catstudy

import java.util.concurrent.Executors

import cats.effect.{ContextShift, Fiber, IO}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object IOMainAsync {

  val ecOne: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val ecTwo: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val csOne: ContextShift[IO] = IO.contextShift(ecOne)
  val csTwo: ContextShift[IO] = IO.contextShift(ecTwo)

  def main(args: Array[String]): Unit = {

//    val prog: IO[Unit] =
//      for {
//        _ <- infiniteIO(1)(csOne)
//        _ <- infiniteIO(11)(csOne)
//      } yield ()

    val prog: IO[Unit] =
      for {
        a <- infiniteIO(1)(csOne)
        //_ <- a.join
        b <- infiniteIO(11)(csOne)
        _ <- b.join
        _ <- infiniteIO(2)(csTwo)
        _ <- infiniteIO(22)(csTwo)
      } yield ()

    println(prog)

    prog.unsafeRunSync()

  }
  import cats.syntax.apply._

  def infiniteIO(id: Int)(implicit cs: ContextShift[IO]): IO[Fiber[IO, Int]] = {
    def repeat: IO[Int] = IO(println(id)).flatMap(_ => IO.shift *> repeat)

    repeat.start(cs)
  }
}

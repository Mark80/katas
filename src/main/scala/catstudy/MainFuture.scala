package catstudy

import java.util.concurrent.Executors

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object MainFuture {

  val ecOne = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val ecTwo = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  def main(args: Array[String]): Unit = {
    val r: Future[Unit] = for {
      _ <- Future({
        println(Thread.currentThread().getName)
        println("zero")
      })

      _ <- Future({
        println(Thread.currentThread().getName)
        Thread.sleep(3000)
        println("uno")
      })(ecOne)

      _ <- Future({
        println(Thread.currentThread().getName)
        println("due")
      })(ecTwo)

    } yield ()

    Await.result(r, 7 seconds)
  }

}

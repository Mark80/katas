package catstudy

import java.util.concurrent.ExecutorService

import cats.effect.IO

import scala.util.Random
import scala.util.control.NonFatal

object AsyncMain {

  def main(args: Array[String]): Unit = {

    val runnable: MyRunnable = new MyRunnable((n: Int) => println(n))

    new Thread(runnable).start()

    Thread.sleep(3000)

  }

  def fork[A](body: => A)(implicit E: ExecutorService): IO[A] =
    IO.async { cb =>
      E.execute(new Runnable {
        def run() =
          try cb(Right(body))
          catch { case NonFatal(t) => cb(Left(t)) }
      })
    }

}

class MyRunnable(callBack: Int => Unit) extends Runnable {
  def run(): Unit = {
    val randomNumber = Random.nextInt(5)
    Thread.sleep(1000)
    callBack(randomNumber)
  }

}

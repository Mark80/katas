package production

object Main {

  def main(args: Array[String]): Unit = {
    println(parallel({ 5 }, { 6 }))
    periodically(500)({ println("ciao") })
    Thread.sleep(10000)
  }

  def periodically(duration: Long)(b: => Unit): Unit =
    new Thread() {
      override def run(): Unit =
        while (true) {
          Thread.sleep(duration)
          b
        }
    }.start()

  def parallel[A, B](a: => A, b: => B): (A, B) = {

    var resultA: Option[A] = None
    var resultB: Option[B] = None

    val thread1 = new Thread() {
      override def run(): Unit = {
        Thread.sleep(1000)
        resultA = Some(a)
      }
    }

    val thread2 = new Thread() {
      override def run(): Unit = {
        Thread.sleep(1000)
        resultB = Some(b)
      }
    }

    thread1.start()
    thread2.start()

    thread1.join()
    thread2.join()

    (resultA.get, resultB.get)

  }

}

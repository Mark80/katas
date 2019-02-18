package production

object Main2 {

  @volatile
  var check: Boolean = true

  def main(args: Array[String]): Unit = {

    val syncVar = new SyncVar[Int]

    val producer = new Thread() {

      var count = 0

      override def run(): Unit = {
        Thread.sleep(100)
        while (count < 16) {
          if (syncVar.isEmpty) {
            syncVar.put(count)
            count = count + 1
            println(s"producer put $count")

          }
          run()
        }
        check = false
      }
    }

    val consumer = new Thread() {
      override def run(): Unit =
        while (check) {
          Thread.sleep(100)
          if (syncVar.nonEmpty) {
            println(s"consumer get ${syncVar.get()}")
          }
          run()
        }

    }
    producer.start()
    consumer.start()

  }

}

class SyncVar[T] {

  @volatile
  private var value: Option[T] = None

  def get(): T = {
    val result = value.getOrElse(throw new RuntimeException)
    value = None
    println("##############")
    result
  }

  def put(x: T): Unit =
    if (value.isEmpty) {
      value = Some(x)
      println(value)
    } else
      throw new RuntimeException

  def isEmpty = value.isEmpty
  def nonEmpty = value.nonEmpty

}

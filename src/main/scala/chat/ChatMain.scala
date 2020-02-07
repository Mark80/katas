package chat

import java.util.concurrent.CountDownLatch

object ChatMain {

  def main(args: Array[String]): Unit = {

    val server = new ChatServer(8080)

    val latch = new CountDownLatch(1)

    server.start()

    Runtime
      .getRuntime()
      .addShutdownHook(
        new Thread() {
          override def run(): Unit = {
            println("shutdown ....")
            server.stop()
          }
        }
      )

    latch.await()

  }

}

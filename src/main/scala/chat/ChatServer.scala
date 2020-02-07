package chat

import java.net.ServerSocket
import java.util.concurrent.atomic.AtomicBoolean

import scala.util.{Failure, Success, Try}

class ChatServer(port: Int) {

  private val controller = new AtomicBoolean(true)
  private val server = new ServerSocket(8080)
  private val clientRepository = new ClientRepository

  def start(): Unit =
    while (controller.get()) {
      acceptClient() match {

        case Success(socketClient) =>
          val channel = new SocketChannel(socketClient)
          val newClient = Client(channel)
          println(s"Added client ${newClient.id}")
          clientRepository.add(newClient)
          new ClientHandler(newClient, clientRepository).start()

        case Failure(_) =>
          stop()
      }
    }

  private def acceptClient() = Try(server.accept())

  def stop(): Unit = {
    clientRepository.getAll.foreach(_.close())
    controller.set(false)
    server.close()
  }

}

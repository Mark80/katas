package chat

import java.io.{BufferedReader, InputStreamReader}
import java.net.Socket
import java.util.UUID

import scala.util.Try

trait Channel {

  def read: Try[String]
  def write(message: String): Unit
  def close(): Unit

}

class SocketChannel(socketClient: Socket) extends Channel {

  def write(message: String): Unit =
    socketClient.getOutputStream.write(message.getBytes)

  def close(): Unit = socketClient.close()

  def read(): Try[String] =
    for {
      buf <- Try(new BufferedReader(new InputStreamReader(socketClient.getInputStream)))
      message <- Try(buf.readLine())
      _ <- Try(buf.close())
    } yield message

}

case class Client(channel: Channel) {

  private val newLine = "\n"

  val id = UUID.randomUUID().toString

  def close(): Try[Unit] =
    Try(channel.close())

  def write(message: String): Unit =
    Try(channel.write(message + newLine))

}

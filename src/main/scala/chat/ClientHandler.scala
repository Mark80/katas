package chat

class ClientHandler(client: Client, repository: ClientRepository) extends Thread {

  @scala.annotation.tailrec
  override final def run(): Unit = {
    broadcastMessage()
    run()
  }

  def broadcastMessage(): Unit = {
    val tryReadMessage = client.channel.read

    tryReadMessage.map { message =>
      repository.getAllOtherClient(client.id).foreach(cli => cli.write(message))
    }.recover {
      case _: Throwable => client.write("an error occurred")
    }
    ()
  }

}

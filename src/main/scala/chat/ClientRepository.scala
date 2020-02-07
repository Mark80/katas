package chat

import scala.collection.mutable.ListBuffer

class ClientRepository {

  private val clients: ListBuffer[Client] = new ListBuffer[Client]

  def add(client: Client): Unit =
    if (!clients.map(_.id).contains(client.id))
      clients += client

  def getAllOtherClient(id: String): List[Client] =
    clients.filter(cli => cli.id != id).toList

  def getAll =
    clients.toList

}

package production

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

class RemoteActor extends Actor with ActorLogging {

  override def receive: Receive = {
    case message: String =>
      println(message)
  }

}

object RemoteActor {

  val conf =
    """
      |akka {
      | 
      | actor {
      |  provider = remote
      | }
      |
      | remote {
      |    enabled-transports = ["akka.remote.netty.tcp"]
      |    netty.tcp {
      |      hostname = "127.0.0.1"
      |      port = 2552
      |      }
      | }
      |}
      |
    """.stripMargin

  def main(args: Array[String]): Unit = {
    val configuration = ConfigFactory.parseString(conf)
    val actorSystem = ActorSystem("remote", configuration)
    val actorLocation = actorSystem.actorOf(Props[RemoteActor], "echo")
    println(actorLocation.path)

  }

}

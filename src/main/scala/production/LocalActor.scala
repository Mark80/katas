package production

import akka.actor.{Actor, ActorLogging, ActorSelection, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

class LocalActor extends Actor with ActorLogging {

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    val remoteActor = context.actorSelection("akka.tcp://remote@127.0.0.1:2552")
    remoteActor ! "hello"
  }

  override def receive: Receive = {

    case message: String =>
      log.info(message)

  }
}

object LocalActor {

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
      |      port = 0
      |      }
      | }
      |}
      |
    """.stripMargin

  def main(args: Array[String]): Unit = {
    val configuration = ConfigFactory.parseString(conf)
    val actorSystem = ActorSystem("remote2", configuration)
    val remoteActor = actorSystem.actorSelection("akka.tcp://remote@127.0.0.1:2552/user/echo")

    remoteActor ! "asdjfgasjfcbhasdcjklasdb"

  }

}

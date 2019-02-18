package birthdayGreetingsKata

import birthdayGreetingsKata.model.Greeting

import scala.concurrent.Future

trait EmailService {
  def sendGreetingsTo(greetings: List[Greeting]): Future[Unit]
}

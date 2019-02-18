package birthdayGreetingsKata.model

import birthdayGreetingsKata.Employ

case class Greeting(email: String, name: String)

object Greeting {

  def from(employ: Employ): Greeting =
    Greeting(employ.email, employ.firstName)

}

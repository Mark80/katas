package birthdayGreetingsKata

import birthdayGreetingsKata.model.{DayOfTheYear, Greeting}

import scala.concurrent.{ExecutionContext, Future}

class BirthdayService {

  def sendGreetings(employeeRepository: EmployeeRepository, emailService: EmailService )(today: DayOfTheYear)(implicit executionContext: ExecutionContext): Future[Unit] =
    for {
      employees <- employeeRepository.findEmployeesBornOn
      _ <- emailService.sendGreetingsTo(createGreetingsFrom(employees))
    } yield ()

  private def createGreetingsFrom(employees: List[Employ]) =
    employees.map(Greeting.from)
}

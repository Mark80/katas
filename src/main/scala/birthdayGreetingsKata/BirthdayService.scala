package birthdayGreetingsKata

import birthdayGreetingsKata.model.{DayOfTheYear, Greeting}

import scala.concurrent.{ExecutionContext, Future}

class BirthdayService(employeeRepository: EmployeeRepository, emailService: EmailService)(implicit executionContext: ExecutionContext) {

  def sendGreetings(today: DayOfTheYear): Future[Unit] =
    for {
      employees <- employeeRepository.findEmployeesBornOn(today.month, today.day)
      _ <- emailService.sendGreetingsTo(createGreetingsFrom(employees))
    } yield ()

  private def createGreetingsFrom(employees: List[Employ]) =
    employees.map(Greeting.from)
}

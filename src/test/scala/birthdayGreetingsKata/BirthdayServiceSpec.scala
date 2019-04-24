package birthdayGreetingsKata

import birthdayGreetingsKata.model.{DayOfTheYear, Greeting}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, Matchers, WordSpec}
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class BirthdayServiceSpec extends WordSpec with Matchers with MockitoSugar with BeforeAndAfterEach with ScalaFutures {

  val employeeRepository = mock[EmployeeRepository]
  val emailService = mock[EmailService]
  val service = new BirthdayService(employeeRepository, emailService)

  override def beforeEach(): Unit =
    reset(employeeRepository, emailService)

  "BirthdayService" should {

    "no send greeting email if no one have a birthday" in {

      val today = DayOfTheYear(month = 11, day = 23)

      when(employeeRepository.findEmployeesBornOn(any[Int], any[Int])).thenReturn(Future.successful(Nil))

      service.sendGreetings(today)
      verify(employeeRepository).findEmployeesBornOn(11, 23)
      verify(emailService).sendGreetingsTo(Nil)
    }

    "send one greeting email if one employ have a birthday" in {

      val today = DayOfTheYear(month = 11, day = 23)

      val email = "email"
      val name = "name"

      val employ = Employ(lastName = "last name", firstName = name, dateOfBirth = today, email = email)
      val greeting = Greeting(email, name)

      when(employeeRepository.findEmployeesBornOn(any[Int], any[Int])).thenReturn(Future.successful(List(employ)))
      when(emailService.sendGreetingsTo(any[List[Greeting]])).thenReturn(Future.unit)

      service.sendGreetings(today).futureValue
      verify(employeeRepository).findEmployeesBornOn(11, 23)
      verify(emailService).sendGreetingsTo(List(greeting))

    }

  }

}

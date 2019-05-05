package birthdayGreetingsKata

import birthdayGreetingsKata.gateway.FileSystemEmployRepository
import birthdayGreetingsKata.model.DayOfTheYear
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext.Implicits.global

class FileSystemEmployRepositorySpec extends WordSpec with Matchers with ScalaFutures {

  val fileSystemEmployRepository = new FileSystemEmployRepository

  "FileSystemEmployRepository" should {

    "read employees from file" in {

      val expectedEmployees =
        List(Employ("Doe", "John", DayOfTheYear(10, 8), "john.doe@foobar.com"),
             Employ("Tosini", "Marco", DayOfTheYear(10, 8), "marco.tosini@foobar.com"))

      val employees = fileSystemEmployRepository.findEmployeesBornOn(10, 8).futureValue

      employees should contain theSameElementsAs expectedEmployees

    }

  }

}

package birthdayGreetingsKata.gateway

import birthdayGreetingsKata.EmployeeRepository
import birthdayGreetingsKata.model.DayOfTheYear
import birthdayGreetingsKata.Employ

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

class FileSystemEmployRepository extends EmployeeRepository {

  private val source = "/Users/mtosini/toy-project/katas/src/test/scala/birthdayGreetingsKata/employ.txt"
  private val employs = getFileLines.map(employFromLine)

  override def findEmployeesBornOn(implicit executionContext: ExecutionContext) : (Mount, Day) =>  Future[List[Employ]] = (month :Mount, day :Day) => Future {
    employs.filter(employ => employ.dateOfBirth == DayOfTheYear(month, day))
  }

  private def getFileLines =
    Source.fromFile(source).getLines().toList

  private def employFromLine(line: String): Employ = {

    val fields = line.split(",")
    val lastName = fields(0).trim
    val firstName = fields(1).trim
    val dateOfBirth = getDayOfTheYear(fields)
    val email = fields(3).trim

    Employ(lastName, firstName, dateOfBirth, email)

  }

  private def getDayOfTheYear(fields: Array[String]) = {

    val dateField = fields(2).split("/")
    val month = dateField(1).toInt
    val day = dateField(2).toInt
    DayOfTheYear(month, day)

  }
}

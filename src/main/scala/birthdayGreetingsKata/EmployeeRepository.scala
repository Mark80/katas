package birthdayGreetingsKata

import scala.concurrent.Future

trait EmployeeRepository {
  def findEmployeesBornOn(month: Int, day: Int): Future[List[Employ]]
}

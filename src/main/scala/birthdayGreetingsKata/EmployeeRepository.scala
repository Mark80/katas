package birthdayGreetingsKata

import scala.concurrent.{ExecutionContext, Future}

trait EmployeeRepository {

  type Mount = Int
  type Day = Int

  def findEmployeesBornOn(implicit executionContext: ExecutionContext): (Mount, Day) =>  Future[List[Employ]]
}

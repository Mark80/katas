package bankaccount

import cats.data.{State, StateT, Writer}
import cats.effect.IO
import org.scalatest.{Matchers, WordSpec}
import cats.implicits._

class BankAccountSpec extends WordSpec with Matchers {

  import AccountAlgebra._

  type TestPrinter[T] = Writer[String, T]

  object TestPrinter {

    def apply[T](s: String, t: T): TestPrinter[T] = Writer(s, t)

  }

  implicit val testPrinter = new Printer[TestPrinter] {
    def print(text: String): TestPrinter[Unit] = Writer.tell(text)
  }

  "As User" should {

    val initialBalance = Transactions(Nil)

    "make a 100 deposit and update balance" in {

      val newBalance = deposit(100).getTransaction(initialBalance)

      newBalance shouldBe Transactions(List(Deposit(100)))

    }

    "make a 100 deposit, 50 deposit and update balance" in {

      val newBalance = (for {
        _ <- deposit(100)
        _ <- deposit(50)
      } yield ()).getTransaction(initialBalance)

      newBalance shouldBe Transactions(List(Deposit(50), Deposit(100)))

    }

    "make a 50 withdraw" in {

      val initialBalance = Transactions(List(Deposit(100)))

      val newBalance = (for {
        _ <- withdraw(50)
      } yield ()).getTransaction(initialBalance)

      newBalance shouldBe Transactions(List(Withdraw(50), Deposit(100)))

    }

    "when make a deposit and withdraw" in {

      val newBalance = (for {
        _ <- deposit(100)
        _ <- withdraw(50)
      } yield ()).getTransaction(initialBalance)

      newBalance shouldBe Transactions(List(Withdraw(50), Deposit(100)))

    }

    "create account balance" in {

      import cats.data._

      val initialBalance = Transactions(List(Withdraw(50), Deposit(100)))

      val newAccountBalance: String = (for {
        _ <- StateT.liftF(TestPrinter("", initialBalance))
        accountBalance <- printStatement[TestPrinter]
      } yield accountBalance).run(initialBalance).run._1

      newAccountBalance shouldBe "withdraw 50\ndeposit 100\n"

    }

  }

  implicit class TestDsl2[A](state: State[Transactions, A]) {

    def getTransaction(initialBalance: Transactions): Transactions =
      state.run(initialBalance).value._1

  }

  implicit class TestDsl[A](state: StateT[IO, Transactions, A]) {

    def getAccountBalance(initialBalance: Transactions): A =
      state.run(initialBalance).unsafeRunSync()._2

  }

}

class TestPrinter extends Printer[IO] {

  def print(text: String): IO[Unit] = IO()
}

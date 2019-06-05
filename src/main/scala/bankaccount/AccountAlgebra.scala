package bankaccount

import cats.Applicative
import cats.data.{State, StateT}
import cats.effect.IO

object AccountAlgebra {

  def deposit(amount: Int): State[Transactions, Unit] =
    State(transactions => (transactions.copy(Deposit(amount) :: transactions.transactions), Unit))

  def withdraw(amount: Int): State[Transactions, Unit] =
    State(transactions => (transactions.copy(Withdraw(amount) :: transactions.transactions), Unit))

  def printStatement[F[_]](implicit printer: Applicative[F]): StateT[F, Transactions, String] =
    StateT(transactions => printer.pure(transactions, AccountBalanceWriter.writeAccountBalance(transactions)))

}

trait Printer[F[_]] {

  def print(text: String): F[Unit]

}

object Printer {

  implicit val ioPrinter: Printer[IO] =
    (text: String) => IO(println(text))

}

object AccountBalanceWriter {

  def writeAccountBalance: Transactions => String =
    transactions =>
      transactions.transactions.foldLeft("") { (current, tx: Transaction) =>
        current + (tx match {
          case Deposit(_)  => s"deposit ${tx.amount}\n"
          case Withdraw(_) => s"withdraw ${tx.amount}\n"
        })
    }

}

sealed trait Transaction {
  def amount: Int
}
case class Deposit(amount: Int) extends Transaction
case class Withdraw(amount: Int) extends Transaction

case class Transactions(transactions: List[Transaction])

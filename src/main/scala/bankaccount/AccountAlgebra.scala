package bankaccount

import cats.Monad
import cats.data.{State, StateT}

object AccountAlgebra {

  def deposit(amount: Int): State[Transactions, Unit] =
    State(transactions => (transactions.copy(Deposit(amount) :: transactions.transactions), Unit))

  def withdraw(amount: Int): State[Transactions, Unit] =
    State(transactions => (transactions.copy(Withdraw(amount) :: transactions.transactions), Unit))

  def printStatement[F[_]](implicit printer: Monad[F]): StateT[F, Transactions, String] =
    StateT(transactions => printer.pure(transactions, AccountBalanceWriter.writeAccountBalance(transactions)))

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

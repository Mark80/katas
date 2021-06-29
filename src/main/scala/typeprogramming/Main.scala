package typeprogramming

import scala.reflect.runtime.universe._


sealed trait Nat

class _0 extends Nat

class Succ[A <: Nat] extends Nat

trait <[A <: Nat, B <: Nat]

object < {

  def apply[A <: Nat, B <: Nat](implicit lt: <[A, B]): A < B = lt

  implicit def baselt[B <: Nat] = new <[_0, Succ[B]] {}

  implicit def inductivelt[B <: Nat, A <: Nat](implicit lt: <[A, B]) = new <[Succ[A], Succ[B]] {}

}

trait Eq[A <: Nat, B <: Nat]

object Eq {

  def apply[A <: Nat, B <: Nat](implicit eq: Eq[A, B]): Eq[A, B] = eq

  implicit def baselt[B <: Nat] = new Eq[_0, _0] {}

  implicit def inductivelt[B <: Nat, A <: Nat](implicit lt: Eq[A, B]) = new Eq[Succ[A], Succ[B]] {}

}


object Main {

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]

  def main(args: Array[String]): Unit = {

    val comp = <[_0, _1]
    val comp2 = <[_2, _3]

    val comp3 = Eq[_0, _0]

    val comp4 = Eq[_2, _2]
    val comp5 = Eq[_2, _2]

  }

}

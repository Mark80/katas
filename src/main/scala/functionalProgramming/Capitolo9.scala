package functionalProgramming

import Prop._

trait Parsers[ParseError, Parser[+ _]] { self =>

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p1)(a => p2.flatMap(b => succeed(f(a, b))))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.flatMap(b => succeed((a, b))))

  implicit class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many(): Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  }
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def productLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      equal(p1 ** p2, map2(p1, p2) { case (a, b) => (a, b) })(in)
  }

}

object Parser {}

object Capitolo9 {

  def main(args: Array[String]): Unit = {}

}

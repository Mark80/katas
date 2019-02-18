package functionalProgramming

import scala.util.Random

trait Monoid[A] {

  def zero: A
  def op(a: A, b: A): A

}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A
}

object Foldable {

  implicit val foldableList = new Foldable[List] {

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))

    override def concatenate[A](as: List[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    val zero = ""
    def op(a1: String, a2: String) = a1 + a2
  }
  val wordsMonoid: Monoid[String] {
    def op(a1: String, a2: String): String

    val zero: String
  } = new Monoid[String] {
    val zero = ""
    def op(a1: String, a2: String) =
      if (!a2.isEmpty)
        a1.trim + " " + a2.trim
      else
        a1
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def zero: WC = Stub("")
    def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c), Stub(d))                 => Stub(c + d)
      case (Stub(c), Part(bleft, n2, bright)) => Part(c + bleft, n2, bright)
      case (Part(aleft, n1, aright), Stub(d)) => Part(aleft, n1, aright + d)
      case (Part(aleft, n1, aright), Part(bleft, n2, bright)) =>
        Part(aleft, n1 + (if ((aright + bleft).isEmpty) 0 else 1) + n2, bright)
    }
  }

  def count(s: String): Int = {

    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    def unstub(a: String) =
      a.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Part(l, count, r) => unstub(l) + count + unstub(r)
      case Stub(a)           => unstub(a)
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {

    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          // The ranges should not overlap if the sequence is ordered.
          case (Just((x1, y1, p)), Just((x2, y2, q))) =>
            Just((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, NONE) => x
          case (NONE, x) => x
        }
      val zero = NONE
    }
    foldMapV(ints, mon)(i => Just((i, i, true))).map(_._3).getOrElse(true)
  }

  def function(a: Int): Int => Boolean =
    b => b < a

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty)
      m.zero
    else if (v.size == 1)
      f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    val zero: List[A] = Nil
    def op(a: List[A], b: List[A]): List[A] = a ++ b
  }
  val intAddition = new Monoid[Int] {
    def zero: Int = 0
    def op(a: Int, b: Int): Int = a + b
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    val zero: Int = 1
    def op(a: Int, b: Int): Int = a * b
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    val zero: Boolean = false
    def op(a: Boolean, b: Boolean): Boolean = a || b
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    val zero: Boolean = true
    def op(a: Boolean, b: Boolean): Boolean = a && b
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero: Option[A] = NONE
    def op(a: Option[A], b: Option[A]): Option[A] = a.orElse(b)
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def zero: A => A = (a: A) => a
    override def op(a: A => A, b: A => A): A => A = a.andThen(b)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as match {
    case Nil          => m.zero
    case head :: tail => m.op(head, concatenate(tail, m))
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  object Law {

    def monoidLaws[A](m: Monoid[A])(gen: Gen[A]): Prop =
      Prop.forAll(gen)(a => m.op(a, m.zero) == a) &&
        Prop.forAll(gen)(a => a == m.op(a, m.zero)) &&
        Prop.forAll(
          for {
            a <- gen
            b <- gen
            c <- gen
          } yield (a, b, c)
        ) {
          case (a, b, c) =>
            m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
        }
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def zero: (A, B) = (ma.zero, mb.zero)
    def op(a: (A, B), b: (A, B)): (A, B) = (ma.op(a._1, b._1), mb.op(a._2, b._2))
  }

}

object Capitolo10 {

  import Monoid._

  val seed: RNG => (Int, RNG) = RNG.positiveMax(100).run
  val intGen: Gen[Int] = Gen(State(seed), MyStream.apply(Just(120), Just(110), Just(600), Just(60), Just(30), Just(150), Just(340)))
  val optionGen: Gen[Option[Int]] = intGen.map {
    case n if n > 100 => NONE
    case n            => Just(n)
  }
  val stringGen = intGen.map {
    case n => Random.alphanumeric.take(n).mkString
  }

  val pairExaustive = MyStream.apply(Just((120, "ciao")),
                                     Just((110, "eccomi")),
                                     Just((600, "super")),
                                     Just((60, "hadoken")),
                                     Just((30, "marco")),
                                     Just((150, "paola")),
                                     Just((340, "ale")))

  def main(args: Array[String]): Unit = {

    val optionProps = Law.monoidLaws(optionMonoid[Int])(optionGen)
    val intProps = Law.monoidLaws(intAddition)(intGen)
    val stringSpaceProps: Prop = Law.monoidLaws(wordsMonoid)(stringGen)

    println(optionProps.run(100, 50, RNG.simple(1)))
    println(intProps.run(100, 50, RNG.simple(1)))
    println(stringSpaceProps.run(100, 50, RNG.simple(1)))

    val a = "OQOlu"
    import Monoid.wordsMonoid.{op, zero}
    println(op(a, zero) == a)
    println(op("Hic", op("est ", "chorda ")) == op(op("Hic ", " est"), "chorda"))
    println(op("Hic", op(zero, "chorda ")) == op(op("Hic ", zero), "chorda"))
    println(concatenate(List("a", "b", "c", "d"), stringMonoid))

    val p1 = Part("lorem", 1, "do")
    val p2 = Part("lor", 2, "")

    Part("lorem", 3, "dolor")
    Part("lorem", 4, "")

    println(wcMonoid.op(p1, p2))
    println(count("lorem ipsum dolor sit amet eccomi"))
    println(count(""))
    println(count("eccomi"))

    println(ordered(IndexedSeq(1, 2, 3, 2, 5, 6)))

    val productGen = (for {
      int <- intGen
      string <- stringGen
    } yield (int, string)).copy(exhaustive = pairExaustive)

    val productProps = Law.monoidLaws(productMonoid(intAddition, wordsMonoid))(productGen)

    println(productProps.run(100, 50, RNG.simple(1)))

  }

}

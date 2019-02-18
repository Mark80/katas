package functionalProgramming

case class Gen[+A](sample: State[RNG, A], exhaustive: MyStream[Option[A]] = MyStream.empty) { self =>

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f), exhaustive.map(value => value.map(f)))

  def map2[B, C](genb: Gen[B])(f: (A, B) => C): Gen[C] =
    flatMap((a: A) => genb.map(b => f(a, b)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap((a: A) => f(a).sample))

  def unsized: SGen[A] =
    SGen(_ => self)

}

object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit[RNG, A](a))

  def int: Gen[Int] =
    Gen(RNG.int)

  def boolean: Gen[Boolean] =
    int.map(_ % 2 == 0)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(bool => if (bool) g1 else g2)

  def uniform: Gen[Double] =
    Gen(State(rng => RNG.double(rng)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val max = stopExclusive - start
    Gen(RNG.positiveMax(max)).map(_ + start)
  }

  def choose(start: Double, stopExclusive: Double): Gen[Double] = {
    val max = stopExclusive - start
    val sample = State(RNG.positiveDoubleMax(max).map(_ + start).run)
    Gen(sample)
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen((0 to n).foldLeft(State.unit[RNG, List[A]](List.empty[A])) {
      case (acc: State[RNG, List[A]], _) =>
        acc.map2(g.sample) {
          case (list, el) => el :: list
        }
    })

}

trait Status
case object Proven extends Status
case object Unfalsified extends Status

import Prop._
case class Prop(run: (MaxCase, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop =
    Prop((n, testCase, rng) =>
      for {
        result1 <- run(n, testCase, rng)
        result2 <- p.run(n, testCase, rng)
      } yield (Unfalsified, result1._2 + result2._2))

  def ||(p: Prop): Prop =
    Prop((max, testCase, rng) =>
      run(max, testCase, rng) match {
        case Left(n)      => p.run(max, testCase, rng)
        case Right(value) => Right(value)
    })
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxCase = Int

  type Result = Either[FailedCase, (Status, SuccessCount)]

  def randomStream[A](g: Gen[A])(rng: RNG): MyStream[A] =
    MyStream.unfold(rng)(rng => Just(g.sample.run(rng)))

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    {

      def loop(from: Int, to: Int, exhaustive: MyStream[Option[A]], onEnd: Int => Result): Result =
        if (from == to) Right((Unfalsified, from))
        else
          exhaustive.uncons match {
            case Just((Just(h), t)) =>
              try {
                if (f(h)) loop(from + 1, to, t, onEnd)
                else Left(h.toString)
              } catch {
                case e: Exception => Left(buildMsg(h, e))
              }
            case Just((NONE, _)) => Right((Unfalsified, from))
            case NONE            => onEnd(from)
          }

      loop(0, n / 3, gen.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(gen)(rng).map(Just(_))
          loop(n / 3, n, rands, i => Right((Unfalsified, i)))
        case s => s
      }
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n - 1) / max + 1
    val props = MyStream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop =
      props
        .map(p =>
          Prop { (max, n, rng) =>
            p.run(max, casesPerSize, rng)
        })
        .toList
        .reduce(_ && _)
    prop.run(max, n, rng)
  }

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

}

case class SGen[+A](forSize: Int => Gen[A]) {

  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

  def map2[B, C](sGen: SGen[B])(f: (A, B) => C): SGen[C] = SGen { n =>
    forSize(n).map2(sGen.forSize(n))((a, b) => f(a, b))
  }

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))

}

object Capitolo8 {

  def main(args: Array[String]): Unit = {

    val seed: RNG => (Int, RNG) = RNG.positiveMax(100).run

    println(Gen.boolean.sample.run(seed(RNG.simple(100))._2))
    println(Gen.choose(3, 20).sample.run(seed(RNG.simple(100))._2))
    println(Gen.choose(3.4, 3.6).sample.run(seed(RNG.simple(100))._2))
    println(Gen.listOfN(10, Gen(State(seed))).sample.run(seed(RNG.simple(100))._2))
    println(Gen.listOfN(10, Gen.uniform).sample.run(seed(RNG.simple(100))._2))
    println(Gen.unit(4).map(el => el * el).sample.run(seed(RNG.simple(100))._2))
    val gen = Gen.choose(3, 20)
    println(Prop.randomStream(gen)(RNG.simple(1)).take(10).toList)

    val gen2 = Gen(State(seed), MyStream.apply(Just(120), Just(110), Just(600), Just(60), Just(30), Just(150), Just(340)))

    val prop1 = Prop.forAll(gen2.unsized)(_ > 200)
    val prop2 = Prop.forAll(gen2.unsized)(_ > 5)

    println((prop1 && prop2).run(100, 5, RNG.simple(10)))
    println((prop1 || prop2).run(100, 5, RNG.simple(10)))

    println(SGen.listOf(gen).forSize(5).sample.run(seed(RNG.simple(100))._2))

  }

}

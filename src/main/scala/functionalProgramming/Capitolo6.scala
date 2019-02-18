package functionalProgramming

import functionalProgramming.RNG._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] =
    State(rng => {
      val (a, rng2) = run(rng)
      (f(a), rng2)
    })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      g(a).run(s2)
    })

}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(rng => (a, rng))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A])) { (el: State[S, A], acc: State[S, List[A]]) =>
      el.map2(acc)(_ :: _)
    }

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def boolean: State[RNG, Boolean] =
    int.map(_ % 2 == 0)

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def positiveInt(rng: RNG): (Int, RNG) =
    int.map {
      case n: Int if n < 0 => -n
      case n               => n
    }.run(rng)

  def double(rng: RNG): (Double, RNG) = {
    val rand: Rand[Double] = State(rng => positiveInt(rng)).map((value: Int) => value.toDouble / Int.MaxValue)
    rand.run(rng)
  }

  def positiveMax(n: Int): Rand[Int] = {
    val rand: Rand[Int] = State(rng => positiveInt(rng))
    rand.map((value: Int) => value % n)
  }

  def positiveDoubleMax(n: Double): Rand[Double] = {
    val rand: Rand[Double] = State(rng => double(rng))
    rand.map((value: Double) => value % n)
  }

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }
}

object Capitolo6 {

  import State._

  def main(args: Array[String]): Unit = {
    val seed: RNG = simple(2)
    println(seed.nextInt)
    println(seed.nextInt)
    println(positiveInt(seed.nextInt._2))
    println(double(simple(100000).nextInt._2.nextInt._2))
    val int: Rand[Int] = State(_.nextInt)
    println(int.run(simple(2)))
    val pRand: Rand[Int] = positiveMax(13)
    println(pRand.run(seed.nextInt._2.nextInt._2.nextInt._2.nextInt._2))
    println(int.flatMap((value: Int) => State(rng => (value * value, rng))).run(seed))
    println(int.map2(int)(_ + _).run(seed))
    println(sequence[RNG, Int](List(unit(5), unit(5))).run(seed))
    println(positiveMax(100).map(_ * 4).run(seed))
    println(boolean.run(seed))

  }

}

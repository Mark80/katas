package trampoline

object MainTrampoline {

  def main(args: Array[String]): Unit = {

    val largeList = (0 to 5001).toList

    val result = for {
      n <- Done(5)
      sq <- square(n)
      r <- addOne(sq)
      r1 <- addOne(r)
      r2 <- addOne(r1)
    } yield r2

    val flat1 = FlatMap(FlatMap(Done(5), (n: Int) => Done(n + 1)), (a: Int) => Done(a.toString))

    val flat2 = FlatMap(Done(5), (n: Int) => FlatMap(Done(n), (n: Int) => Done(n + 1)))

    //println(flat1.runT)

    val hello: Trampoline[Unit] = for {
      _ <- More(() => Done(print("Hello, ")))
      _ <- More(() => Done(println("World!")))
    } yield ()

    (hello zip hello).runT

  }

  def square(n: Int): Trampoline[Int] =
    More(() => Done(n * n))

  def addOne(n: Int): Trampoline[Int] =
    More(() => Done(n + 1))

  def even[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil     => Done(true)
      case _ :: xs => More(() => odd(xs))
    }

  def odd[A](ns: List[A]): Trampoline[Boolean] =
    ns match {
      case Nil     => Done(false)
      case x :: xs => More(() => even(xs))
    }

  def getState[S]: StateM[S, S] =
    StateM(s => Done(s, s))
  def setState[S](s: S): StateM[S, Unit] =
    StateM(_ => Done(s, ()))
  def pureState[S, A](a: A): StateM[S, A] =
    StateM(s => Done(s, a))

  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(
        pureState[Int, List[(Int, A)]](List())
      )((acc, a) =>
        for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
        } yield (n, a) :: xs)
      .runS(0)
      .runT
      ._2
      .reverse

}

case class StateM[S, A](runS: S => Trampoline[(S, A)]) {

  def map[B](f: A => B): StateM[S, B] =
    StateM(s => {
      val (newState, value) = runS(s).runT
      Done((newState, f(value)))
    })

  def flatMap[B](f: A => StateM[S, B]): StateM[S, B] = StateM[S, B](
    runS andThen { tramp =>
      FlatMap[(S, A), (S, B)](tramp, { case (s, a) => f(a).runS(s) })
    }
  )

}

sealed trait Trampoline[+A] {

  def map[B](f: A => B): Trampoline[B] =
    flatMap(a => Done(f(a)))

  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
    FlatMap(this, (a: A) => f(a))

  def zip[B](b: Trampoline[B]): Trampoline[(A, B)] = (this.resume, b.resume) match {
    case (Right(a), Right(b)) => Done((a, b))
    case (Left(a), Left(b))   => More(() => a() zip b())
    case (Left(a), Right(b))  => More(() => a() zip Done(b))
    case (Right(a), Left(b))  => More(() => Done(a) zip b())
  }

  final def resume: Either[() => Trampoline[A], A] = this match {

    case Done(v) =>
      Right(v)

    case More(thunk) =>
      Left(thunk)

    case FlatMap(sub: Trampoline[A], f: (A => Trampoline[A])) =>
      sub match {

        case FlatMap(b, g) =>
          (FlatMap(b, (x: Any) => FlatMap(g(x), f)): Trampoline[A]).resume

        case Done(v) =>
          f(v).resume

        case More(k) =>
          Left(() => FlatMap(k(), f))
      }

  }

  @scala.annotation.tailrec
  final def runT: A = resume match {
    case Right(a) =>
      a
    case Left(k) =>
      k().runT
  }

}
case class Done[+A](value: A) extends Trampoline[A]
case class More[+A](thunk: () => Trampoline[A]) extends Trampoline[A]
case class FlatMap[A, +B](t: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

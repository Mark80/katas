package trampoline

object ADTMain {

  def getState[S]: StateS[S, S] = StateS(s => Pure(s, s))
  def setState[S](s: S): StateS[S, Unit] = StateS(_ => Pure((), s))
  def pureState[S, A](a: A): StateS[S, A] = StateS(s => Pure(a, s))

  def zipIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(pureState[Int, List[(Int, A)]](List())) { (acc: StateS[Int, List[(Int, A)]], a) =>
        for {
          xs <- acc
          n <- getState
          _ <- setState(n + 1)
        } yield (n, a) :: xs
      }
      .runS(0)
      .runT
      ._1
      .reverse

  def main(args: Array[String]): Unit = {
    println(zipIndex((0 to 5000).toList))

    val program = Bind(Bind(Pure(5), (n: Int) => Pure(n + 1)), (n: Int) => Pure(n * n))

    Bind(Pure(5), (n: Int) => Bind(Pure(n + 1), (n: Int) => Pure(n * n)))
    Bind(Pure(6), (n: Int) => Pure(n * n))
    Pure(36)

    val result = for {
      n <- pure(5)
      adOne <- addOne(n)
      squar <- square(adOne)
    } yield squar

    println(result.runT)

    println(program.runT)

  }

  def addOne(n: Int): Eval[Int] =
    pure(n).flatMap(n => Pure(n + 1))

  def square(n: Int): Eval[Int] =
    pure(n).flatMap(n => Pure(n * n))

  def pure(n: Int): Eval[Int] = Pure(n)

}

trait Eval[+A] {

  def flatMap[B](f: A => Eval[B]): Eval[B] = this match {
    case Bind(a, g) =>
      Bind(a, (x: Any) => g(x) flatMap f)
    case x => Bind(x, f)
  }

  def map[B](f: A => B): Eval[B] =
    this.flatMap(x => Pure(f(x)))

  final def runT: A = this match {
    case Pure(v)     => v
    case Map(sub, f) => sub.map(f).runT
    case Delay(k)    => k().runT
    case Bind(sub, f) =>
      sub match {
        case Pure(v)  => f(v).runT // g(f(d(f(x))))
        case Delay(k) => k().flatMap(f).runT
        case Bind(b, g) =>
          b.flatMap((x: Any) => g(x) flatMap f).runT

      }

  }

}
case class Pure[+A](value: A) extends Eval[A]
case class Delay[A](thunk: () => Eval[A]) extends Eval[A]
case class Bind[A, +B](eval: Eval[A], f: A => Eval[B]) extends Eval[B]
case class Map[A, +B](eval: Eval[A], f: A => B) extends Eval[B]

case class StateS[S, +A](runS: S => Eval[(A, S)]) {

  def map[B](f: A => B): StateS[S, B] =
    flatMap(x => ADTMain.pureState(f(x)))

  def flatMap[B](f: A => StateS[S, B]): StateS[S, B] =
    StateS[S, B](s =>
      Delay(() =>
        runS(s) flatMap {
          case (a, s1) => Delay(() => f(a) runS s1)
      }))
}

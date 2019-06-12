package trampoline

object ADTMain {

  def main(args: Array[String]): Unit = {

    val program = Bind(Bind(Bind(Pure(5), (n: Int) => Pure(n + 1)), (n: Int) => Pure(n * n)), (n: Int) => Pure(n.toString + "pippo"))

    Bind(Pure(5), (n: Int) => Bind(Pure(n + 1), (n: Int) => Pure(n * n)))
    Bind(Pure(6), (n: Int) => Pure(n * n))
    Pure(6 * 6)

    println(program.resume)

  }

}

sealed trait Eval[+T] {

  def flatMap[B](f: T => Eval[B]): Eval[B] = this match {
    case Bind(a, g) =>
      Bind(a, (x: Any) => g(x) flatMap f)
    case x => Bind(x, f)
  }

  final def resume: T = this match {
    case Pure(v) => v
    case Bind(eval, f) =>
      eval match {
        case Pure(v) => f(v).resume
        case Bind(sub, g) =>
          Bind(sub, (x: Any) => g(x).flatMap(f)).resume
      }

  }

}
case class Bind[A, +B](value: Eval[A], f: A => Eval[B]) extends Eval[B]
case class Pure[+A](value: A) extends Eval[A]

package functionalProgramming

import functionalProgramming.Par.Par

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

}

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(b => g(b))

  def id[A](a: A): A = a

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(id)

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft(unit(List.empty[A])) { (acc, ma) =>
      map2(acc, ma) { (a, b) =>
        b :: a
      }
    }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft(unit(List.empty[B])) { (acc, a) =>
      map2(acc, f(a)) { (a, b) =>
        b :: a
      }
    }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] =
    e match {
      case Left(mb)  => map(mb)(b => Left(b))
      case Right(ma) => map(ma)(a => Right(a))
    }

}

object IntStateMonad extends Monad[({ type IntState[A] = State[Int, A] })#IntState] {
  def unit[A](a: => A): State[Int, A] = ???
  def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]): State[Int, B] = ???
}

object Monad {

  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val optionMonad = new Monad[Option] {

    override def unit[A](a: => A): Option[A] = Just(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val parMonad = new Monad[Par] {

    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type lambda[x] = State[S, x] })#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}

object Functor {

  implicit val listFunctor = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  def unzip[F[_], A, B](fab: F[(A, B)])(implicit functor: Functor[F]): (F[A], F[B]) =
    (functor.map(fab)(ab => ab._1), functor.map(fab)(ab => ab._2))

}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val function: F[B => C] = map(fa)(f.curried)
    apply(function)(fb)
  }

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  def unit[A](a: A): F[A]
}

case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] =
    Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)

}

case class Reader[R, A](run: R => A)

object Reader {

  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader((r: R) => f(st.run(r)).run(r))

  }
}

object Capitolo11 {

  import Monad._

  def main(args: Array[String]): Unit = {

    val listTuple = List((1, "String"), (2, "String 2"), (3, "String 3"))
    println(Functor.unzip(listTuple))

    println(optionMonad.sequence[Int](List(Just(75), Just(65), Just(25), Just(15), Just(53))))

    println(optionMonad.cofactor[String, Int](Right(Just(5))))

  }

}

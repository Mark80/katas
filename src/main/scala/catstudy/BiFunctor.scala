package catstudy

import cats.{Functor, Monad}

trait BiFunctor[F[_, _]] {

  def bimap[A, B, C, D](f: A => C)(g: B => D): F[A, B] => F[C, D]

  def firstProjeton[A, B, C, D](f: A => C): F[A, B] => F[C, B] =
    bimap(f)(identity[B])

  def secondProjeton[A, B, C, D](g: B => D): F[A, B] => F[A, D] =
    bimap(identity[A])(g)

  def identity[A]: A => A = (a: A) => a

}

object BiFunctor {

  implicit val tuple2Bifunctor: Tuple2BiFunctor = new Tuple2BiFunctor
}

class Tuple2BiFunctor extends BiFunctor[Tuple2] {
  def bimap[A, B, C, D](f: A => C)(g: B => D): ((A, B)) => (C, D) = {
    case (a, b) => (f(a), g(b))
  }
}

case class Identity[A](value: A)

object Identity {

  implicit val functorId: Functor[Identity] = new Functor[Identity] {
    def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity[B](f(fa.value))
  }

}

case class KleisliCat[F[_], A, B](run: A => F[B]) {

  import cats.implicits._

  def >=>[C](g: B => F[C])(implicit ev: Monad[F]): KleisliCat[F, A, C] =
    KleisliCat((a: A) => run(a).flatMap(g))

}

object Cat {

  type Reader[A, B] = A => B

  object Reader {
    implicit def readerFunctor[R]: Functor[({
      type T[X] = Reader[R, X]
    })#T] = new Functor[({ type T[X] = Reader[R, X] })#T] {
      def map[A, B](fa: Reader[R, A])(f: A => B): Reader[R, B] = ???
    }

  }

}

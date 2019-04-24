package catstudy

import scala.language.higherKinds

trait MonadM[F[_]] {

  def pure[A](a: A) : F[A]
  def flatMap[A,B](a : F[A])(f : A => F[B]) : F[B]
  def map[A,B](fa : F[A])(f : A => B) : F[B] =
    flatMap(fa)(a => pure(f(a)))


}

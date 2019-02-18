package functionalProgramming

trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def isEmpty: Boolean
}

case class Just[+A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Just(f(get))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  override def getOrElse[B >: A](default: => B): B = get
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
  override def filter(f: A => Boolean): Option[A] =
    if (f(get))
      this
    else
      NONE
  override def isEmpty: Boolean = false
}

case object NONE extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = NONE
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = NONE
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  override def filter(f: Nothing => Boolean): Option[Nothing] = NONE
  override def isEmpty: Boolean = true
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}
case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = Right(value)
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
    case Left(a)      => Left(a)
    case Right(right) => Right(f(value, right))

  }
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

object Capitolo4 {

  def main(args: Array[String]): Unit = {

    val option = Just(3)

    println(option.map(_ + 1))

    println(sequence(Seq(Just(1))))

    val result: Either[String, Int] =
      for {
        a <- Right(1)
        _ <- Right(5)
        b <- Right(2 + a)
      } yield b

    println(result)

  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      value1 <- a
      value2 <- b
    } yield f(value1, value2)

  def sequence[A](values: Seq[Option[A]]): Option[Seq[A]] =
    if (values.isEmpty)
      NONE
    else {
      values.tail.foldLeft(values.head.map(el => Seq(el))) { (acc: Option[Seq[A]], mayBe: Option[A]) =>
        mayBe.flatMap(element => acc.map(seq => Seq(element).++:(seq)))
      }

    }

}

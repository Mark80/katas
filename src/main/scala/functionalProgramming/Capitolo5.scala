package functionalProgramming

trait MyStream[+A] {
  def uncons: Option[(A, MyStream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def toList: List[A]
  def take(n: Int): MyStream[A]
  def takeWhile(p: A => Boolean): MyStream[A]
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Just((h, t)) => f(h, t.foldRight(z)(f))
      case NONE         => z
    }
  def constant[A](a: A): MyStream[A]

  def map[B](f: A => B): MyStream[B] = uncons match {
    case Just((h, t)) => MyStream.cons(f(h), t.map(f))
    case NONE         => MyStream.empty[B]
  }

  def zip[B](s: MyStream[B]): MyStream[(A, B)] =
    uncons match {
      case Just((h, t)) =>
        s.uncons match {
          case Just((h1, t1)) => MyStream.cons((h, h1), t.zip(t1))
          case NONE           => MyStream.empty[(A, B)]
        }
      case NONE => MyStream.empty[(A, B)]
    }

}
object MyStream {
  def empty[A]: MyStream[A] =
    new MyStream[A] {
      def uncons = NONE
      def toList: List[A] = Nil
      def take(n: Int): MyStream[A] = MyStream.empty
      def takeWhile(p: A => Boolean): MyStream[A] = MyStream.empty
      def constant[A](a: A): MyStream[A] = MyStream.empty
    }
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = new MyStream[A] {
    def uncons: Option[(A, MyStream[A])] = Just(hd, tl)
    def toList: List[A] = uncons match {
      case Just((head, tail)) => head :: tail.toList
      case NONE               => Nil
    }
    def take(n: Int): MyStream[A] =
      uncons match {
        case Just((head, tail)) if n > 0 => cons(head, tail.take(n - 1))
        case _                           => empty
      }

    def takeWhile(p: A => Boolean): MyStream[A] =
      uncons match {
        case Just((head, tail)) if p(head) => cons(head, tail.takeWhile(p))
        case _                             => empty
      }
    def constant[A](a: A): MyStream[A] = cons(a, constant(a))
  }

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def from(n: Int): MyStream[Int] = cons(n, from(n + 1))

  def fib(start: Int = 0, end: Int = 1): MyStream[Int] = cons(start, fib(end, start + end))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] =
    f(z) match {
      case NONE                    => empty
      case Just((value, newState)) => cons(value, unfold(newState)(f))
    }

  def startsWith[A](s: MyStream[A], s2: MyStream[A]): Boolean =
    (s.uncons, s2.uncons) match {
      case (Just((value, tail)), Just((value2, tail2))) if value == value2 => startsWith(tail, tail2)
      case (Just((_, _)), NONE) | (NONE, NONE)                             => true
      case _                                                               => false

    }
}

object Capitolo5 {

  def main(args: Array[String]): Unit = {

    val stream = MyStream.apply(1, 2, 3, 4)
    val stream2 = MyStream.apply(1, 2, 3, 4, 5)

    println(stream.toList)
    println(stream.take(3).toList)
    println(stream.takeWhile(_ < 5).toList)
    println(stream.foldRight(0)(_ + _))
    println(stream.constant(5).take(10).toList)
    println(MyStream.from(5).take(10).toList)
    println(MyStream.fib().take(5).toList)
    println(MyStream.unfold(0)(value => Just(value, value + 1)).take(10).toList)
    println(MyStream.startsWith(stream, stream2))
    println(MyStream.from(5).take(10).map(_ + 100).toList)

  }
}

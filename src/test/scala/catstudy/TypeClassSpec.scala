package catstudy

import cats.data.{Reader, Validated}
import cats.effect.IO
import cats.{Functor, Monad}
import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec
import scala.language.higherKinds

class TypeClassSpec extends WordSpec with Matchers {

  val cat = Cat("name", 3, "blu")

  "Printable" should {
    import Printable._
    import PrintableInstance._
    import PrintableSyntax._

    "work as type class" in {

      format(cat) shouldBe "name is a 3 old blu cat"

    }

    "work with syntax" in {

      cat.format shouldBe "name is a 3 old blu cat"

    }

  }

  "Show's cats" should {

    "works" in {

      import cats.Show

      implicit val catsShow = new Show[Cat] {
        def show(cat: Cat): String =
          s"${cat.name} is a ${cat.age} old ${cat.color} cat"

      }
      val showCats = Show.apply[Cat]

      showCats.show(cat) shouldBe s"${cat.name} is a ${cat.age} old ${cat.color} cat"

    }

  }

  "Variance" should {

    class Foo[-A] {
      override def toString: String =
        "Foo"
    }

    trait B
    class C extends B

    def foo[A](implicit ev: Foo[A]) =
      println(ev)

    class FooSub extends Foo[C] {
      override def toString: String =
        "FooSub"
    }

    "select subtype" in {

      implicit val instance2 = new FooSub
      implicit val instance = new Foo[B]

      foo[B]

    }

  }

  "Function" should {

    "behave as a functor" in {
      import cats.syntax.functor._
      import cats.instances.function._

      val double = (n: Int) => n * 2

      val pulsOne = (n: Int) => n + 1

      val func =
        ((x: Int) => x.toDouble).map(x => x + 1).map(x => x * 2).map(x => x + "!")

      func(123) shouldBe "248.0!"

    }

  }

  "Functor" should {

    "work for different type constructor" in {

      import cats.Functor
      import cats.instances.list._
      import cats.instances.option._

      val l = List(1, 2, 3, 4)

      val result = Functor[List].map(l)(_ + 2)

      result shouldBe List(3, 4, 5, 6)

      doMath(Option(3)) shouldBe Some(8)

    }

  }

  "Functor Tree" should {

    "works" in {

      implicit val treeFunctor = new Functor[Tree] {

        def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
          fa match {
            case Leaf(value) =>
              Leaf(f(value))
            case Branch(left, right) =>
              Branch(map(left)(f), map(right)(f))

          }
      }

      val tree = Branch(Leaf(5), Leaf(7))
      Functor[Tree].map(tree)(_ + 5) shouldBe Branch(Leaf(10), Leaf(12))

    }

  }

  "Box" should {

    "call contramap" in {

      implicit val printableInt = new Printable[Int] {
        def format(a: Int): String = s"The number is $a"
      }

      val box = Box(5)
      implicit val boxPrintable = printableBox(box)

      Printable.format(box) shouldBe "The number is 5"

    }

    def printableBox[A](box: Box[A])(implicit env: Printable[A]): Printable[Box[A]] =
      env.contramap[Box[A]](b => b.value)

  }

  "Monad" should {

    "works" in {
      import cats.instances.option._

      val result: Option[Int] = genericMethod(Option(3), Option(5))

      println(result)

    }

    "monad error as well" in {

      import cats.MonadError
      import cats.instances.either._ // for MonadError
      val monadError = MonadError[ErrorOr, String]

      val result: ErrorOr[Int] = monadError.pure(5)

      val errorResult: ErrorOr[Nothing] = monadError.raiseError("error")

    }

    "eval as well" in {

      import cats.Eval

      var acc = ""
      var acc2 = ""

      //def
      val always = Eval.always({
        acc = acc + "eccomi"
        5
      })

      //val
      val now = Eval.now({
        acc2 = acc2 + "eccomi"
        5
      })

      always.value
      always.value

      now.value
      now.value

      acc shouldBe "eccomieccomi"
      acc2 shouldBe "eccomi"

    }

    "ReaderMonad" should {

      "works" in {

        val catName: Reader[Cat, String] = Reader((cat: Cat) => cat.name)

        val greetKitty: Reader[Cat, String] =
          catName.map(name => s"Hello ${name}")

        greetKitty.run(cat) // res1: cats.Id[String] = Hello Heathcliff

        val helloCat = Reader((s: Cat) => "Hello")

        val result = for {
          name <- catName
          hello <- helloCat
        } yield s"$hello $name"

        result.run(cat) shouldBe "Hello name"

      }

      "semigroupal" in {

        import cats.Semigroupal
        import cats.instances.option._

        val product: Option[(Int, Int)] = Semigroupal[Option].product(Some(5), Some(6))

        product shouldBe Some((5, 6))

      }

      "validated" in {

        import cats.Semigroupal
        import cats.data.Validated
        import cats.instances.list._

        Semigroupal[AllErrorsOr].product(
          Validated.invalid(List("error1")),
          Validated.invalid(List("error2"))
        )

      }

      "IO" in {
        IO.pure(5)
        val ioPrint = IO(println("IO"))

        val io: IO[Unit] = for {
          _ <- ioPrint
          _ <- ioPrint
        } yield ()

        io.unsafeRunSync()

      }

      "Trampoline" in {

        def even[A](lst: Seq[A]): Trampoline[Boolean] =
          lst match {
            case Nil =>
              println("even Done")
              Done(true)
            case _ :: xs =>
              println("even More")
              Suspend(() => odd(xs))
          }

        def odd[A](lst: Seq[A]): Trampoline[Boolean] =
          lst match {
            case Nil =>
              println("odd Done")
              Done(false)
            case _ :: xs =>
              println("odd More")
              Suspend(() => even(xs))
          }

        println(even((0 to 5).toList))

      }

    }

  }

  type AllErrorsOr[A] = Validated[List[String], A]

  type DbReader[A] = Reader[Db, A]

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader((db: Db) => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader((db: Db) => db.passwords.get(username).contains(password))

  import cats.syntax.applicative._

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      userName <- findUsername(userId)
      isOK <- userName.map(name => checkPassword(name, password)).getOrElse(false.pure[DbReader])
    } yield isOK

  type ErrorOr[A] = Either[String, A]

  def genericMethod[F[_]: Monad](fa: F[Int], fb: F[Int]): F[Int] = {
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    for {
      a <- fa
      b <- fb
    } yield a + b
  }

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = {
    import cats.syntax.functor._
    start.map(n => (n + 1) * 2)
  }

}

final case class Box[A](value: A)

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

sealed trait Trampoline[+A] {

  def resume: Either[() => Trampoline[A], A] = this match {
    case Done(v)    => Right(v)
    case Suspend(k) => Left(k)
    case FlatMap(sub: Trampoline[A], cont: (A => Trampoline[A])) =>
      sub match {
        case Done(v)    => cont(v).resume
        case Suspend(k) => Left(() => FlatMap(k(), cont))
        case FlatMap(sub2, cont2) =>
          (FlatMap(sub2, (x: Any) => FlatMap(cont2(x), cont)): Trampoline[A]).resume
      }
  }

  @tailrec
  final def run: A =
    this match {
      case Done(v: A) => v
      case Suspend(t) => t().run // <- tail recursive, yay
    }

  @tailrec
  final def runT: A = resume match {
    case Right(value) => value
    case Left(more)   => more().runT
  }

}
case class Done[A](result: A) extends Trampoline[A]
case class Suspend[A](call: () => Trampoline[A]) extends Trampoline[A]
case class FlatMap[A, B](sub: Trampoline[A], cont: A => Trampoline[B]) extends Trampoline[B]

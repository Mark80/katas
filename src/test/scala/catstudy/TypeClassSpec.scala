package catstudy

import java.io.{BufferedReader, File, FileReader}
import java.util.concurrent.{Executors, ScheduledExecutorService, ScheduledFuture, ScheduledThreadPoolExecutor, TimeUnit}

import cats.data.{Reader, Validated}
import cats.effect.{IO, Timer}
import cats.{Functor, Monad}
import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.language.higherKinds
import scala.util.{Failure, Success}

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
              Done(true)
            case _ :: xs =>
              Suspend(() => odd(xs))
          }

        def odd[A](lst: Seq[A]): Trampoline[Boolean] =
          lst match {
            case Nil =>
              Done(false)
            case _ :: xs =>
              Suspend(() => even(xs))
          }

      }

      "from IO async" in {

        def convert[A](fa: => Future[A])(implicit ec: ExecutionContext): IO[A] =
          IO.async { cb =>
            // This triggers evaluation of the by-name param and of onComplete,
            // so it's OK to have side effects in this callback
            fa.onComplete {
              case Success(a) => cb(Right(a))
              case Failure(e) => cb(Left(e))
            }
          }
        import scala.concurrent.ExecutionContext.Implicits.global

        val io = convert {
          Future {
            println("ecccomi")
            5
          }
        }

        val io2 = convert {
          Future {
            throw new RuntimeException("Errorrrrrrr!!!!!!")
          }
        }

        runAsync(io)
        runAsync(io2)

        def delayedTick(d: FiniteDuration)(implicit sc: ScheduledExecutorService): IO[Unit] =
          IO.cancelable { cb =>
            val runnable = new Runnable { def run() = cb(Right(234)) }
            val f: ScheduledFuture[_] = sc.schedule(runnable, d.length, d.unit)

            // Returning the cancellation token needed to cancel
            // the scheduling and release resources early
            IO(f.cancel(false))
          }

        implicit val exService: ScheduledExecutorService = Executors.newScheduledThreadPool(1)

        runAsync(delayedTick(FiniteDuration(2, TimeUnit.SECONDS)))

        //Thread.sleep(3000)

        val sio = IO.suspend {
          io
        }

        import cats.implicits._
        import cats.effect.ContextShift

        def fib(n: Int, a: Long, b: Long)(implicit cs: ContextShift[IO]): IO[Long] =
          IO.suspend {
            if (n == 0) IO.pure(a)
            else {
              val next = fib(n - 1, b, a + b)
              // Every 100 cycles, introduce a logical thread fork
              if (n % 100 == 0)
                cs.shift *> next
              else
                next
            }
          }

      }

      "Resource" in {

        import cats.effect._
        import cats.implicits._

        val file = "/Users/mtosini/toy-project/katas/src/test/scala/catstudy/TypeClassSpec.scala"

        val acquireExt: IO[Source] = IO {
          scala.io.Source.fromFile(file)
        }

        def mkResource(s: String): Resource[IO, Source] = {
          val acquire: IO[Source] =
            IO(println(s"Acquiring $s")) *> acquireExt

          def release(s: Source) =
            IO(s.close())

          Resource.make(acquire)(release)
        }

        mkResource(file).use((source: Source) => IO(throw new RuntimeException)).unsafeRunSync()

        Resource.fromAutoCloseable(acquireExt).use((source: Source) => IO(println(source.mkString))).unsafeRunSync()

      }

    }

    "cancel io " in {

      import cats.implicits._

      // Needed for `sleep`
      import scala.concurrent.duration._
      implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

      // Delayed println
      val io3: IO[Unit] = IO.sleep(10.seconds) *> IO(println("Hello!"))

      val cancel: IO[Unit] =
        io3.unsafeRunCancelable(r => println(s"Done: $r"))

      // ... if a race condition happens, we can cancel it,
      // thus canceling the scheduling of `IO.sleep`
      cancel.unsafeRunSync()

      def readFirstLine(file: File): IO[String] =
        IO(new BufferedReader(new FileReader(file))).bracket { in =>
          // Usage (the try block)
          IO(in.readLine())
        } { in =>
          // Releasing the reader (the finally block)
          IO(in.close())
        }

      import cats.implicits._
      import cats.effect.ContextShift

      def readFile(file: File)(implicit cs: ContextShift[IO]): IO[String] = {
        // Opens file with an asynchronous boundary before it,
        // ensuring that processing doesn't block the "current thread"
        val acquire: IO[BufferedReader] = IO.shift *> IO(new BufferedReader(new FileReader(file)))

        acquire.bracket { in =>
          // Usage (the try block)
          IO {
            // Ugly, low-level Java code warning!
            val content = new StringBuilder()
            var line: String = null
            do {
              line = in.readLine()
              if (line != null) content.append(line)
            } while (line != null)
            content.toString()
          }
        } { in =>
          // Releasing the reader (the finally block)
          // This is problematic if the resulting `IO` can get
          // canceled, because it can lead to data corruption
          IO(in.close())
        }
      }

    }

    "context shift" in {

      import cats.effect.{ContextShift, IO}
      import scala.concurrent.ExecutionContext.Implicits.global

      implicit val cs: ContextShift[IO] = IO.contextShift(global)

      def loop(n: Int, task: => Unit): IO[Unit] =
        if (n > 0)
          IO.shift.flatMap(_ => {
            println(task)
            loop(n - 1, task)
          })
        else
          IO.unit

      val io: IO[Unit] = loop(8, { Thread.sleep(500); println(Thread.currentThread().getName) })

      io.unsafeRunSync()

    }

  }

  private def runAsync[T](io: IO[T]) =
    io.unsafeRunAsync {
      case Right(value) => println(value)
      case Left(ex)     => println(ex.getMessage)
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

class IOT[+A](val unsafeInterpret: () => A) { s =>
  def map[B](f: A => B) = flatMap(f.andThen(IOT.effect(_)))
  def flatMap[B](f: A => IOT[B]): IOT[B] =
    IOT.effect(f(s.unsafeInterpret()).unsafeInterpret())
}
object IOT {
  def effect[A](eff: => A) = new IOT(() => eff)
}

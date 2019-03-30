package cats

case class Cat(name: String, age: Int, color: String)

trait Printable[A] {

  def format(a: A): String

  def print[A](a: A): Unit =
    print(format(a))

}

object Printable {

  def format[A](a: A)(implicit printable: Printable[A]): String =
    printable.format(a)

  def print[A](a: A)(implicit printable: Printable[A]): Unit =
    println(format(a))

}

object PrintableInstance {

  implicit val catPrintable = new Printable[Cat] {
    def format(cat: Cat): String =
      s"${cat.name} is a ${cat.age} old ${cat.color} cat"
  }

}

object PrintableSyntax {

  implicit class PrintableOps[A](a: A)(implicit ev: Printable[A]) {

    def format = ev.format(a)
    def print = ev.print(a)

  }

}

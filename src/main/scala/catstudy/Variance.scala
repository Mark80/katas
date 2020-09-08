package catstudy

object Variance {

  class Animal {
    def stuff: Unit = println("Animal")
  }

  class Cat extends Animal {
    override def stuff: Unit = println("Cat")

    def catStuff: Unit = println("only cat")

  }

  trait SoundMaker[-T] {

    def foo(t: T)

  }

  implicit object AnimalSound extends SoundMaker[Animal] {
    def foo(t: Animal): Unit = {
      t.stuff
      println(123)
    }
  }

  def makeSound[T](implicit soundMaker: SoundMaker[T]) = ()

  makeSound[Animal]
  makeSound[Cat]

}

package functionalProgramming

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => UnitFuture[A](a)

  def async[A](a: => A): Par[A] = fork(unit(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = executorService => {
    val first = a(executorService)
    val second = b(executorService)
    UnitFuture(f(first.get(), second.get()))
  }

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = es => unit(f(fa(es).get()))(es)

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = es => {
    l.foldRight(unit(List.empty[A])) { (a: A, acc: Par[List[A]]) =>
      map2(acc, unit(a))((acc: List[A], value: A) => if (f(value)) value :: acc else acc)
    }(es)
  }

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] = es => {
    val first = fa(es)
    val second = fb(es)
    async((first.get(), second.get()))(es)
  }

  def parMapOld[A, B](l: List[A])(f: A => B): Par[List[B]] =
    es => async(l.map(f))(es)

  def asyncF[A, B](f: A => B): A => Par[B] =
    (value: A) => async(f(value))

  def sequence[A](l: List[Par[A]]): Par[List[A]] = es => {
    l.foldRight(unit(List.empty[A])) { (a: Par[A], acc: Par[List[A]]) =>
      map2(acc, a)((el: List[A], par: A) => par :: el)
    }(es)
  }

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def fork[A](a: => Par[A]): Par[A] = executor => executor.submit(() => run(a)(executor).get())

  def join[A](aa: Par[Par[A]]): Par[A] = es => {
    run(run(aa)(es).get())(es)
  }

  def choice[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = {
    val is = map(a)(bool => if (bool) 0 else 1)
    choiceN(is)(List(ifTrue, ifFalse))

  }

  def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val b: Par[A] = flatMap(a)((index: Int) => choices(index))
    b(es)

  }

  def choiceMap[A, B](a: Par[A])(choices: Map[A, Par[B]]): Par[B] = es => {
    val b: Par[B] = flatMap(a)(a => choices(a))
    b(es)
  }

  def flatMap[A, B](a: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val b: Par[B] = join(map(a)((index: A) => choices(index)))
    b(es)
  }

  def run[A](a: Par[A])(implicit executorService: ExecutorService): Future[A] = a(executorService)

}

object Capitolo7 {

  implicit val executorService: ExecutorService = Executors.newFixedThreadPool(8)

  import Par._

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      println(l)
      println(r)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }

  def main(args: Array[String]): Unit = {
    println(run(map2(async(5), async(4))(_ + _)).get())
    println(run(sum(IndexedSeq(1, 2, 3, 4))).get())
    val asyncFunction = asyncF((value: Int) => value * value)
    println(run(asyncFunction(3)).get())
    println(run(map(async(3))(_ + 1)).get())
    println(run(product(async(3), async(6))).get())
    println(run(parFilter(List(1, 2, 3, 4, 6))(_ % 2 == 0)).get())
    println(run(unit(5)))
    println(run(choice(unit(false))(unit(15), unit(6))).get())
    println(run(choiceN(unit(1))(List(unit(15), unit(26)))).get())

    executorService.awaitTermination(1000, TimeUnit.MILLISECONDS)
    executorService.shutdown()
  }

}

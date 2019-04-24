package catstudy

import cats.Applicative

import scala.language.higherKinds


object CaseStudyOne {


  def main(args: Array[String]): Unit = {


  }
}

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UpTimeService[F[_]](client: UptimeClient[F]) {

  import cats.instances.list._
  import cats.syntax.functor._
  import cats.syntax.traverse._


  def getTotalUpTime(hostNames: List[String])(implicit applicative: Applicative[F]): F[Int] = {
    val value: F[List[Int]] = hostNames.traverse(host => client.getUptime(host))
    value.map(_.sum)
  }

}

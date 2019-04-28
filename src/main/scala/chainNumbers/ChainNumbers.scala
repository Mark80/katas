package chainNumbers

import scala.annotation.tailrec

object ChainNumbers {

  @tailrec
  def chain(n: Int, rings: Set[Int] = Set.empty): Int = {
    val diff = desc(n) - asc(n)
    if (rings.contains(diff))
      rings.size + 1
    else
      chain(diff, rings + diff)

  }

  def desc(n: Int): Int =
    ordering(n, _ > _)

  def asc(n: Int): Int =
    ordering(n, _ < _)

  private def ordering(n: Int, order: (Int, Int) => Boolean): Int =
    n.toString.toList
      .map(_.toString)
      .map(_.toInt)
      .sortWith(order)
      .mkString
      .toInt

}

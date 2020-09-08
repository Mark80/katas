package artofcomputerprogramming

object EuclideAlgorithm {

  def mcd(m: Int, n: Int, r: Int = 0, s: Int = 1): Int =
    (m, n, r, s) match {
      case (_, _, _, 1) => mcd(m, n, m % n, 2)
      case (_, _, _, 2) => if (r == 0) n else mcd(m, n, r, 3)
      case (_, _, _, 3) => mcd(n, r, r, 1)
      case _            => -1
    }

  def main(args: Array[String]): Unit =
    println(mcd(544, 119))
}

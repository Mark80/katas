package tree

object BinaryTree {

  import Tree._

  def main(args: Array[String]): Unit = {
    println(cBalanced(3, "x"))
    println(cBalanced(4, "x").size)

  }

}

object Tree {

  def cBalanced[T](n: Int, t: T): List[Tree[T]] = n match {

    case _ if n == 0 => List(End)

    case _ if (n - 1) % 2 == 0 =>
      cBalanced((n - 1) / 2, t).flatMap((tree: Tree[T]) => List(Node(t, tree, tree)))

    case _ if (n - 1) % 2 == 1 =>
      val r = for {
        big <- cBalanced(n / 2, t)
        small <- cBalanced((n - 1) / 2, t)
      } yield List(Node(t, small, big), Node(t, big, small))
      r.flatten

  }

}

sealed abstract class Tree[+T]
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}
case object End extends Tree[Nothing] {
  override def toString = "."
}
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

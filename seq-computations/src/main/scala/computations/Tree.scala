package computations

sealed trait Tree[A] {
  def fold[B](acc: B, f: (B, A) => B): B = {
    this match {
      case Leaf(a) => f(acc, a)
      case Node(left, right) => left.fold(right.fold(acc, f), f)
    }
  }

  def concat: String = {
    fold[String]("", (acc: String, elem: A) => elem + " " + acc).trim
  }

  def sum[C >: A <: Int]: Int = {
    fold[Int](0, (acc: Int, elem: C) => elem + acc)
  }
}

final case class Leaf[A](value: A) extends Tree[A]
final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

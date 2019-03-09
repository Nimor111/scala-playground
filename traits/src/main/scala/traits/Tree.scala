package traits

sealed trait Tree {
  def sum: Int = {
    this match {
      case Leaf(x)           => x
      case Node(left, right) => left.sum + right.sum
    }
  }

  def double: Tree = {
    this match {
      case Leaf(x) => Leaf(x * 2)
      case Node(left, right) => Node(left.double, right.double)
    }
  }
}
final case class Leaf(elem: Int) extends Tree
final case class Node(left: Tree, right: Tree) extends Tree

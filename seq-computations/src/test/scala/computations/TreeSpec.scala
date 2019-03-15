package computations

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  "The Tree object concat method" should "concatenate the elements of the tree" in {
    val tree = Node(Node(Leaf("To"), Leaf("iterate")),
               Node(Node(Leaf("is"), Leaf("human,")),
               Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

    tree.concat shouldEqual "To iterate is human, to recurse divine"
  }

  "The tree object sum method" should "sum tree elements" in {
    val tree = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))

    tree.sum shouldEqual 10
  }
}

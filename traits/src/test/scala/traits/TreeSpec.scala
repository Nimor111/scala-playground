package traits

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  "Tree sum" should "return sum of elements in tree" in {
    Node(Node(Leaf(3), Leaf(3)), Leaf(1)).sum shouldEqual 7
  }

  it should "return value for leaf" in {
    Leaf(2).sum shouldEqual 2
  }

  "Tree double" should "return double of tree leaf values" in {
    Node(Node(Leaf(3), Leaf(3)), Leaf(1)).double shouldEqual Node(Node(Leaf(6), Leaf(6)), Leaf(2))
  }

  it should "return doubled value of leaf" in {
    Leaf(2).double shouldEqual Leaf(4)
  }
}

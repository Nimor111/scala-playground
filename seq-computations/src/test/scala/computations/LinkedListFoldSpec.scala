package computations

import org.scalatest._

class LinkedListFoldSpec extends FlatSpec with Matchers {
  "The LinkedListFold object length method" should "calculate length of list" in {
    val example = PairF(1, PairF(2, PairF(3, EndF)))
    example.length shouldEqual 3
    example.tail.length shouldEqual 2
  }

  it should "return 0 for empty list" in {
    EndF.length shouldEqual 0
  }

  "The LinkedListFold object sum method" should "calculate sum of list" in {
    val example = PairF(1, PairF(2, PairF(3, EndF)))
    example.sum shouldEqual 6
    example.tail.sum shouldEqual 5
  }

  it should "return 0 for empty list" in {
    EndF.sum shouldEqual 0
  }

  "The LinkedListFold object product method" should "calculate product of list" in {
    val example = PairF(1, PairF(2, PairF(3, EndF)))
    example.product shouldEqual 6
    example.tail.product shouldEqual 6
  }

  it should "return 0 for empty list" in {
    EndF.product shouldEqual 1
  }
}

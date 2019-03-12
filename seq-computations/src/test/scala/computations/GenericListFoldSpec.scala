package computations

import org.scalatest._

class GenericListFoldSpec extends FlatSpec with Matchers {
  "The GenericListFold object length method" should "calculate length of list" in {
    val example = PairF(1, PairF(2, PairF(3, EndF())))
    example.length shouldEqual 3
    example.tail.length shouldEqual 2
  }

  it should "return 0 for empty list" in {
    EndF().length shouldEqual 0
  }
}

package computations

import org.scalatest._

class GenericListSpec extends FlatSpec with Matchers {
  "The GenericList object length method" should "calculate length of list" in {
    val example = Pair(1, Pair(2, Pair(3, End())))
    example.length shouldEqual 3
    example.tail.length shouldEqual 2
  }

  it should "return 0 for empty list" in {
    End().length shouldEqual 0
  }

  "The GenericList object contains method" should "find values correctly" in {
    val example = Pair(1, Pair(2, Pair(3, End())))
    example.contains(2) shouldEqual true
    example.contains(4) shouldEqual false
  }

  it should "not find anything in empty list" in {
    End().contains(3) shouldEqual false
  }

  "The GenericList object apply method" should "return nth item " in {
    val example = Pair(1, Pair(2, Pair(3, End())))
    example.apply(2) shouldEqual Success(3)
    example.apply(4) shouldEqual Failure("Element not found!")
  }

  it should "not find anything" in {
    val example = End()
    example.apply(3) shouldEqual Failure("Element not found!")
  }
}

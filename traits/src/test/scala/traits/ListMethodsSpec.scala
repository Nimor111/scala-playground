package traits

import org.scalatest._

class ListMethodsSpec extends FlatSpec with Matchers {
  "IntList length" should "return length of list" in {
    Cons(1, Cons(2, Cons(3, End))).length shouldEqual 3
  }

  it should "return 0 on empty list" in {
    End.length shouldEqual 0
  }

  it should "calculate tail length" in {
    val list = Cons(1, Cons(2, Cons(3, End)))
    list.tail.length shouldEqual 2
  }

  "IntList product" should "return product of list" in {
    Cons(1, Cons(2, Cons(3, End))).product shouldEqual 6
  }

  it should "return 1 on empty list" in {
    End.product shouldEqual 1
  }

  it should "calculate tail product" in {
    Cons(1, Cons(2, Cons(3, End))).tail.product shouldEqual 6
  }

  "IntList double" should "double each element of list" in {
    Cons(1, Cons(2, Cons(3, End))).double shouldEqual Cons(
      2,
      Cons(4, Cons(6, End))
    )
  }

  it should "return same list on empty list" in {
    End.double shouldEqual End
  }

  it should "calculate double of tail" in {
    Cons(1, Cons(2, Cons(3, End))).tail.double shouldEqual Cons(4, Cons(6, End))
  }
}

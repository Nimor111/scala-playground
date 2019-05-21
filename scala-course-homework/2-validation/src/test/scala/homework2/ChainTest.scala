package homework2

import org.scalatest.{ FlatSpec, Matchers }

class ChainTest extends FlatSpec with Matchers {
  "++" should "append two chains" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldEqual Chain(1, 2, 3, 4)
  }

  "+:" should "prepend an element to the chain" in {
    1 +: Singleton(2) shouldEqual Append(Singleton(1), Singleton(2))
    1 +: Append(Singleton(1), Singleton(2)) shouldEqual Append(
      Singleton(1),
      Append(Singleton(1), Singleton(2)))
  }

  ":+" should "append an element to the chain" in {
    Singleton(2) :+ 1 shouldEqual Append(Singleton(2), Singleton(1))
    Append(Singleton(1), Singleton(2)) :+ 1 shouldEqual Append(
      Singleton(1),
      Append(Singleton(2), Singleton(1)))
  }

  "listify" should "transform a chain" in {
    val actual = Append(
      Append(Singleton(1), Singleton(2)),
      Append(Singleton(3), Singleton(4))).listify
    val expected =
      Append(
        Singleton(1),
        Append(Singleton(2), Append(Singleton(3), Singleton(4))))
    actual shouldEqual expected
  }

  "map, foldLeft, foldMap" should "transform a chain correctly" in {
    val chain = Append(
      Append(Singleton(1), Singleton(2)),
      Append(Singleton(3), Singleton(4)))
    val expected = Append(
      Append(Singleton(2), Singleton(3)),
      Append(Singleton(4), Singleton(5)))

    chain.map(_ + 1) shouldEqual expected
    chain.foldLeft(0)(_ + _) shouldEqual 10
    chain.flatMap(a => Singleton(a + 1)) shouldEqual expected
  }

  "operations" should "work with Singleton chains" in {
    val chain = Singleton(1)

    chain :+ 2 shouldEqual Append(Singleton(1), Singleton(2))
    2 +: chain shouldEqual Append(Singleton(2), Singleton(1))
    chain ++ Singleton(2) shouldEqual Append(Singleton(1), Singleton(2))
    chain ++ Append(Singleton(2), Singleton(3)) shouldEqual Append(
      Singleton(1),
      Append(Singleton(2), Singleton(3)))
    chain.listify shouldEqual Singleton(1)
    chain.map(_ + 1) shouldEqual Singleton(2)
    chain.flatMap(a => Singleton(a + 1)) shouldEqual Singleton(2)
    chain.foldLeft(0)(_ + _) shouldEqual 1
  }

  it should "work with Append chains" in {
    val chain = Append(Singleton(1), Singleton(2))

    chain :+ 3 shouldEqual Append(
      Singleton(1),
      Append(Singleton(2), Singleton(3)))
    3 +: chain shouldEqual Append(
      Singleton(3),
      Append(Singleton(1), Singleton(2)))
    chain ++ Singleton(3) shouldEqual Append(
      Singleton(1),
      Append(Singleton(2), Singleton(3)))
    chain ++ Append(Singleton(3), Singleton(4)) shouldEqual Append(
      Singleton(1),
      Append(Singleton(2), Append(Singleton(3), Singleton(4))))
    chain.map(_ + 1) shouldEqual Append(Singleton(2), Singleton(3))
    chain.flatMap(a => Singleton(a + 1)) shouldEqual Append(
      Singleton(2),
      Singleton(3))
    chain.foldLeft(0)(_ + _) shouldEqual 3
  }
}

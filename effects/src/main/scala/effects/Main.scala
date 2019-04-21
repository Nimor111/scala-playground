package effects

object Main {
  def main(args: Array[String]) = {
    val list: MyList[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Empty))))
    print(list.prepend(5))
  }
}


package homework1

object Functions {
  def fromDigits(digits: List[Int], radix: Int = 10): Int = {
    val indices = (0 to digits.size - 1).toList
    def calculate(pair: (Int, Int)): Int = {
      val pos = digits.size - 1 - pair._2
      pair._1 * scala.math.pow(radix, pos).toInt
    }

    // digits.zipWithIndex.map(calculate).fold(0)(_ + _)
    myFold(0, myMap(myZip(digits, indices), calculate))(_ + _)
  }

  def myMap[T, U](xs: List[T], f: T => U): List[U] = {
    xs match {
      case List()  => List()
      case x :: xs => f(x) :: myMap(xs, f)
    }
  }

  def myZip[T, U](xs: List[T], ys: List[U]): List[(T, U)] = {
    xs match {
      case List() => List()
      case x :: xs => {
        ys match {
          case List()  => List()
          case y :: ys => (x, y) :: myZip(xs, ys)
        }
      }
    }
  }

  def myFold(acc: Int, xs: List[Int])(f: (Int, Int) => Int): Int = {
    xs match {
      case List()  => acc
      case x :: xs => f(x, myFold(acc, xs)(f))
    }
  }

  def parseInteger(integer: String, radix: Int = 10): Int = {
    def parseChar(chr: Char): Int = {
      chr match {
        case c if c.isLetter => c.toInt - 55
        case '-'             => -1
        case c               => c.asDigit
      }
    }

    val parsed = myMap(integer.toList, chr => parseChar(chr))

    parsed match {
      case -1 :: xs => -fromDigits(xs, radix)
      case xs       => fromDigits(xs, radix)
    }
  }

  def zipMap(a: List[Int], b: List[Int], f: (Int, Int) => Int): List[Int] = {
    myMap(myZip(a, b), (t: (Int, Int)) => f(t._1, t._2))
  }

  def countCoinChangeVariants(denominations: List[Int], change: Int): Int = {
    // it's either in the subset or it's not in the subset
    if (change == 0) 1
    else if (change < 0 || denominations.isEmpty) 0
    else
      countCoinChangeVariants(denominations, change - denominations.head) + countCoinChangeVariants(
        denominations.tail,
        change)
  }

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue =
    ???
}

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
      case List() => List()
      case x :: xs => f(x) :: myMap(xs, f)
    }
  }

  def myZip[T, U](xs: List[T], ys: List[U]): List[Tuple2[T, U]] = {
    xs match {
      case List() => List()
      case x :: xs => {
        ys match {
          case List() => List()
          case y :: ys => (x, y) :: myZip(xs, ys)
        }
      }
    }
  }

  def myFold(acc: Int, xs: List[Int])(f: (Int, Int) => Int): Int = {
    xs match {
      case List() => acc
      case x :: xs => f(x, myFold(acc, xs)(f))
    }
  }

  def parseInteger(integer: String, radix: Int = 10): Int = {
    def parseChar(chr: Char): Int = {
      if (chr.isLetter) {
        return chr.toInt - 55
      }

      if (chr == '-') {
        return -1
      }

      return chr.toInt - 48
    }

    val parsed = myMap(integer.toList, chr => parseChar(chr))

    parsed match {
      case -1 :: xs => -fromDigits(xs, radix)
      case xs => fromDigits(xs, radix)
    }
  }

  def zipMap(a: List[Int], b: List[Int], f: (Int, Int) => Int): List[Int] = {
    myMap(a.indices.toList, (index: Int) => f(a(index), b(index)))
  }

  def genSizeCombination(n: Int, l: List[Int]): List[List[Int]] = {
    n match {
      case 0 => List(List())
      case _ => for {
        el <- l
        sl <- genSizeCombination(n - 1, l dropWhile { _ != el })
      } yield el :: sl
    }
  }

  def countCoinChangeVariants(denominations: List[Int], change: Int): Int = {
    def gen(n: Int) = {
      for {
        c <- genSizeCombination(n, denominations) if c.sum == change
      } yield c
    }

    val indices = (1 to change).toList

    (for {
      n <- indices
      combs <- gen(n)
    } yield combs).size
  }

  def bfsTraversal(start: Int, end: Int, neighbours: Int => List[Int]): Queue = ???
}

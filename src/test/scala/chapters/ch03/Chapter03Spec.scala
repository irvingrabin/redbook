package chapters.ch03

import chapters.ch03.lib.{List, Cons, Nil}
import org.scalatest.{FlatSpec, Matchers}

class Chapter03Spec extends FlatSpec with Matchers {
  it should "run test 03_01" in {
    val x = List[Int](1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case _ => 101
    }

    x shouldBe 3
  }

  it should "run test 03_02" in {
    List().tail shouldBe Nil
    List(1).tail shouldBe Nil
    List(1, 2, 3, 4, 5).tail shouldBe List(2, 3, 4, 5)
  }

  it should "run test 03_03" in {
    def replaceHead[A] = (h: A, l: List[A]) => l match {
      case Cons(a, t) => Cons(h, t)
      case _          => Cons(h, Nil)
    }
    replaceHead(5, List()) shouldBe List(5)
    replaceHead(5, List(1)) shouldBe List(5)
    replaceHead(5, List(1, 2, 3, 4, 5)) shouldBe List(5, 2, 3, 4, 5)
  }

  it should "run test 03_04" in {
    def dropHeads[A](n: Int, l: List[A]): List[A] =
      if (n <= 0) l
      else l match {
        case Cons(a, t) => dropHeads(n - 1, t)
        case _          => Nil
      }

    dropHeads(5, List()) shouldBe Nil
    dropHeads(1, List(1)) shouldBe Nil
    dropHeads(3, List(1, 2, 3, 4, 5)) shouldBe List(4, 5)
  }

  it should "run test 03_05" in {
    val isEven = (n: Int) => (n % 2 == 0)
    val isTreven = (n: Int) => (n % 3 == 0)

    List().dropWhile(isEven) shouldBe Nil
    List(1).dropWhile(isEven) shouldBe List(1)
    List(3, 9, 2, 4, 5).dropWhile(isTreven) shouldBe List(2, 4, 5)
    List(3, 9, 3, 24, 45).dropWhile(isTreven) shouldBe Nil
  }

  it should "run test 03_06" in {
    List(1, 2, 3).init shouldBe List(1, 2)
    List(1, 2).init shouldBe List(1)
    List(1).init shouldBe List()
    List().init shouldBe List()
  }

  it should "run test 03_07" in {
    // TODO: needs to be done
  }

  it should "run test 03_08" in {
    // TODO: needs to be done
  }

  it should "run test 03_09" in {
    // TODO: needs to be done
  }

  it should "run test 03_10" in {
    // TODO: needs to be done
  }

  it should "run test 03_11" in {
    // TODO: needs to be done
  }

  it should "run test 03_12" in {
    // TODO: needs to be done
  }

  it should "run test 03_13" in {
    // TODO: needs to be done
  }

  it should "run test 03_14" in {
    // TODO: needs to be done
  }

  it should "run test 03_15" in {
    // TODO: needs to be done
  }

  it should "run test 03_16" in {
    // TODO: needs to be done
  }

  it should "run test 03_17" in {
    // TODO: needs to be done
  }

  it should "run test 03_18" in {
    // TODO: needs to be done
  }

  it should "run test 03_19" in {
    // TODO: needs to be done
  }

  it should "run test 03_20" in {
    // TODO: needs to be done
  }

  it should "run test 03_21" in {
    // TODO: needs to be done
  }

  it should "run test 03_22" in {
    // TODO: needs to be done
  }

  it should "run test 03_23" in {
    // TODO: needs to be done
  }

  it should "run test 03_24" in {
    // TODO: needs to be done
  }

  it should "run test 03_25" in {
    // TODO: needs to be done
  }

  it should "run test 03_26" in {
    // TODO: needs to be done
  }

  it should "run test 03_27" in {
    // TODO: needs to be done
  }

  it should "run test 03_28" in {
    // TODO: needs to be done
  }

  it should "run test 03_29" in {
    // TODO: needs to be done
  }
}

package chapters.ch03

import chapters.ch03.lib.{Branch, Cons, Leaf, List, Nil, Tree}
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
    List.sum(List(1, 2, 3, 4)) shouldBe 10
    List.sum(List(1, 2)) shouldBe 3
    List.sum(List()) shouldBe 0
    List.sum2(List(1, 2, 3, 4)) shouldBe 10
    List.sum2(List(1, 2)) shouldBe 3
    List.sum2(List()) shouldBe 0

    List.product(List(1.0, 2.0, 3.0, 4.0)) shouldBe 24.0
    List.product(List(1.0, 2.0)) shouldBe 2.0
    List.product(List(1.0, 2.0, 0.0, 5.0, 8.0, 11.0)) shouldBe 0.0
    List.product(List[Int]()) shouldBe 1.0
    List.product2(List(1.0, 2.0, 3.0, 4.0)) shouldBe 24.0
    List.product2(List(1.0, 2.0)) shouldBe 2.0
    List.product2(List(1.0, 2.0, 0.0, 5.0, 8.0, 11.0)) shouldBe 0.0
    List.product2(List[Double]()) shouldBe 1.0
    List.product3(List(1.0, 2.0, 3.0, 4.0)) shouldBe 24.0
    List.product3(List(1.0, 2.0)) shouldBe 2.0
    List.product3(List(1.0, 2.0, 0.0, 5.0, 8.0, 11.0)) shouldBe 0.0
    List.product3(List[Double]()) shouldBe 1.0
  }

  it should "run test 03_08" in {
    List(1, 2, 3).foldRight(Nil:List[Int])(Cons(_, _)) shouldBe List(3, 2, 1)
  }

  it should "run test 03_09" in {
    List(1, 2, 3).length shouldBe 3
  }

  it should "run test 03_10" in {
    List().length shouldBe 0
  }

  it should "run test 03_11" in {
    val x1 = List(1, 2, 3, 4, 5, 6)
    x1.print("Got list")
    println(s" length of x1 is ${x1.length}")
    println(s"    sum of x1 is ${List.sum(x1)}")
    println(s"product of x1 is ${List.product(x1)}")  }

  it should "run test 03_12" in {
    val x1 = List(1, 2, 3, 4, 5, 6)
    x1.print(
      "  original x1 is")
    List.reverse(x1).print(
      "reverse of x1 is")
  }

  it should "run test 03_13" in {
    // TODO: needs to be done
  }

  it should "run test 03_14" in {
    val x1 = List(1, 2, 3, 4, 5, 6)

    x1.print(
      "                x1 is")
    x1.append(7).print(
      "x1 with appended 7 is")
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
    var t0 = Leaf(26)
    var t1 = Leaf(66)
    println(s"t0 has size ${t0.size}")
    println(s"t1 has size ${t1.size}")
    var t2 = Branch(t0, t1)
    println(s"t2 has size ${t2.size}")
    var t3 = Branch(t0, t2)
    println(s"t3 has size ${t3.size}")

    t0.size shouldBe 1
    t1.size shouldBe 1
    t2.size shouldBe 2
    t3.size shouldBe 3
  }

  it should "run test 03_26" in {
    def max(tree: Tree[Int]): Int = {
      def _max(t: Tree[Int], m: Int): Int = t match {
        case Leaf(x) => if (x > m) x else m
        case Branch(l, r) => {
          val lmax = _max(l, m)
          val rmax = _max(r, m)
          if (lmax > rmax) lmax else rmax
        }
      }
      _max(tree, Int.MinValue)
    }
    var t0 = Leaf(26)
    var t1 = Leaf(66)
    println(s"t0 has size ${t0.size}")
    println(s"t1 has size ${t1.size}")
    var t2 = Branch(t0, t1)
    println(s"t2 has size ${t2.size}")
    var t3 = Branch(t0, t2)
    println(s"t3 has size ${t3.size}")
    println(s"t3 has maximum ${max(t3)}")
    var t4 = Leaf(71)
    var t5 = Branch(t4, t3)
    println(s"t5 has maximum ${max(t5)}")
  }

  it should "run test 03_27" in {
    // TODO: Move operations to Tree
    def max(tree: Tree[Int]): Int = {
      def _max(t: Tree[Int], m: Int): Int = t match {
        case Leaf(x) => if (x > m) x else m
        case Branch(l, r) => {
          val lmax = _max(l, m)
          val rmax = _max(r, m)
          if (lmax > rmax) lmax else rmax
        }
      }
      _max(tree, Int.MinValue)
    }

    def depth(tree: Tree[Int]): Int = {
      def _depth(t: Tree[Int], m: Int): Int = t match {
        case Leaf(x) => m + 1
        case Branch(l, r) => {
          val lmax = _depth(l, m + 1)
          val rmax = _depth(r, m + 1)
          if (lmax > rmax) lmax else rmax
        }
      }
      _depth(tree, 0)
    }

    var t0 = Leaf(26)
    var t1 = Leaf(66)
    var t2 = Branch(t0, t1)
    var t3 = Branch(t0, t2)
    var t4 = Leaf(71)
    var t5 = Branch(t4, t3)
    var t6 = Branch(t4, t5)

    depth(t0) shouldBe 1
    depth(t1) shouldBe 1
    depth(t2) shouldBe 2
    depth(t3) shouldBe 3
    depth(t4) shouldBe 1
    depth(t5) shouldBe 4
    depth(t6) shouldBe 5
  }

  it should "run test 03_28" in {
    val t0 = Leaf(26)
    val t1 = Leaf(66)
    val t2 = Branch(t0, t1)
    val t3 = Leaf(49)
    val t4 = Leaf(71)
    val t5 = Branch(t4, t3)
    val t6 = Leaf(99)
    val t7 = Branch(t6, t5)
    val t8 = Branch(t2, t7)

    val t9 = t8.map(100 - _)
    t9 shouldBe Branch(
      Branch(
        Leaf(74),
        Leaf(34)
      ),
      Branch(
        Leaf(1),
        Branch(
          Leaf(29),
          Leaf(51)
        )
      )
    )
  }

  it should "run test 03_29" in {
    val t0 = Leaf(26)
    val t1 = Leaf(66)
    val t2 = Branch(t0, t1)
    val t3 = Leaf(29)
    val t4 = Leaf(71)
    val t5 = Branch(t4, t3)
    val t6 = Leaf(99)
    val t7 = Branch(t6, t5)
    val t8 = Branch(t2, t7)

    t8.print
    println(s"Size of t8 is ${t8.size}")
    //println(s"Maxi of t0 is ${t0.max}")
    //println(s"Maxi of t8 is ${t8.max}")
  }
}

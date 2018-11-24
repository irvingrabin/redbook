package chapters.ch03

import library.{Tree, Branch, Leaf}

object Ex_03_27 extends App {
  println("Ex_03_27")

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
  println(s"t0 has depth ${depth(t0)}")
  var t1 = Leaf(66)
  println(s"t1 has depth ${depth(t1)}")
  var t2 = Branch(t0, t1)
  println(s"t2 has depth ${depth(t2)}")
  var t3 = Branch(t0, t2)
  println(s"t3 has depth ${depth(t3)}")
  var t4 = Leaf(71)
  println(s"t4 has depth ${depth(t4)}")
  var t5 = Branch(t4, t3)
  println(s"t5 has depth ${depth(t5)}")
  var t6 = Branch(t4, t5)
  println(s"t6 has depth ${depth(t6)}")

}

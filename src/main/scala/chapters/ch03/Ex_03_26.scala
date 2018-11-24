package chapters.ch03

import library.{Tree, Leaf, Branch}

object Ex_03_26 extends App {
  println("Ex_03_26")

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

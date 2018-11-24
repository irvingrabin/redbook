package chapters.ch03

import library.{Tree, Leaf, Branch}

object Ex_03_28 extends App {
  println("Ex_03_28")

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
  val t9 = t8.map(100 - _)
  t9.print
  val t10 = t9.map(x => x * x)
  t10.print
}

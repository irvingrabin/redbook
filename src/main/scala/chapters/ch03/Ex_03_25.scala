package chapters.ch03

import library.{Leaf, Branch}

object Ex_03_25 extends App {
  println("Ex_03_25")

  var t0 = Leaf(26)
  var t1 = Leaf(66)
  println(s"t0 has size ${t0.size}")
  println(s"t1 has size ${t1.size}")
  var t2 = Branch(t0, t1)
  println(s"t2 has size ${t2.size}")
  var t3 = Branch(t0, t2)
  println(s"t3 has size ${t3.size}")

}

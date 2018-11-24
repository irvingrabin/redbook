package chapters.ch03

import library.{List}

object Ex_03_11 extends App {
  println("Ex_03_11")

  val x1 = List(1, 2, 3, 4, 5, 6)
  x1.print("Got list")
  println(s" length of x1 is ${x1.length}")
  println(s"    sum of x1 is ${List.sum(x1)}")
  println(s"product of x1 is ${List.product(x1)}")
}

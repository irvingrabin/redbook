package chapters.ch03

import lib.List

object Ex_03_12 extends App {
  println("Ex_03_12")

  val x1 = List(1, 2, 3, 4, 5, 6)
  x1.print(
    "  original x1 is")
  List.reverse(x1).print(
    "reverse of x1 is")
}

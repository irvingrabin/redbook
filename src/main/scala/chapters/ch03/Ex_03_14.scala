package chapters.ch03

import lib.List

object Ex_03_14 extends App {
  println("Ex_03_14")

  val x1 = List(1, 2, 3, 4, 5, 6)

  x1.print(
    "                x1 is")
  x1.append(7).print(
    "x1 with appended 7 is")
}

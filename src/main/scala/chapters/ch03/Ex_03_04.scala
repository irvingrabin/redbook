package chapters.ch03

import library._

object Ex_03_04 extends App {
  println("Ex_03_04")

  def dropHeads[A](n: Int, l: List[A]): List[A] =
    if (n <= 0) l
    else l match {
      case Cons(a, t) => dropHeads(n - 1, t)
      case _          => Nil
    }

  assert(dropHeads(5, List()) == Nil)
  assert(dropHeads(1, List(1)) == Nil)
  assert(dropHeads(3, List(1, 2, 3, 4, 5)) == List(4, 5))
}
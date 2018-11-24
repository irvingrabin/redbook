package chapters.ch03

import library.{List, Cons, Nil}

object Ex_03_02 extends App {
  println("Ex_03_02")

  def tail[A] = (l: List[A]) => l match {
    case Cons(a, t) => t
    case _          => Nil
  }
  assert(tail(List()) == Nil)
  assert(tail(List(1)) == Nil)
  assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
}
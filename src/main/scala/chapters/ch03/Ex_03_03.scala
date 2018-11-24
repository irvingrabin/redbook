package chapters.ch03

import library.{List, Cons, Nil}

object Ex_03_03 extends App {
  println("Ex_03_03")

  def replaceHead[A] = (h: A, l: List[A]) => l match {
    case Cons(a, t) => Cons(h, t)
    case _          => Cons(h, Nil)
  }
  assert(replaceHead(5, List()) == List(5))
  assert(replaceHead(5, List(1)) == List(5))
  assert(replaceHead(5, List(1, 2, 3, 4, 5)) == List(5, 2, 3, 4, 5))
}
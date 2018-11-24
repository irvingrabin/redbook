package chapters.ch03

import library._

object Ex_03_05 extends App {
  println("Ex_03_05")

  def isEven(n: Int): Boolean = (n % 2 == 0)
  def isTreven(n: Int): Boolean = (n % 3 == 0)

  def dropWhile[A](f: A => Boolean, l: List[A]): List[A] =
    l match {
      case Cons(a, t) => if (f(a)) dropWhile(f, t) else l
      case _          => Nil
    }

  assert(dropWhile(isEven, List()) == Nil)
  assert(dropWhile(isEven, List(1)) == List(1))
  assert(dropWhile(isTreven, List(3, 9, 2, 4, 5)) == List(2, 4, 5))
  assert(dropWhile(isTreven, List(3, 9, 3, 24, 45)) == Nil)
}
package chapters.ch03

import lib.{List, Cons, Nil}

object Ex_03_08 extends App {
  println("Ex_03_08")

  val x1 = List(1, 2, 3).foldRight(Nil:List[Int])(Cons(_, _))
  println(x1)
}

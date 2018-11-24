package chapters.ch03

import library._

object Ex_03_06 extends App {
  println("Ex_03_06")

  def len[A](l: List[A]): Int = l match {
    case Cons(x, xs) => 1 + len(xs)
    case Nil         => 0
  }

  def append[A](item: A, l: List[A]): List[A] = l match {
    case Cons(x, xs) => Cons(x, append(item, xs))
    case Nil         => List(item)

  }

  def init[A](l: List[A]): List[A] = {
    def _init(hl: List[A], tl: List[A]): List[A] = tl match {
      case Cons(tlh, tls @ Cons(_, _)) => _init(append(tlh, hl), tls)
      case _                           => hl
    }
    _init(Nil, l)
  }

  val testCases = Seq(
    (List(1, 2, 3), List(1, 2)),
    (List(1, 2), List(1)),
    (List(1), List()),
    (List(), List()),
  )

  testCases.map {
    case (source, result) => {
      Predef.println("TESTCASE:")
      source.print("Source")
      result.print("Expected Result")
      init(source).print("Real Result")
    }
  }
}
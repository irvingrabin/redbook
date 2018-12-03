package chapters.ch05

import chapters.ch03.{List, Cons => ListCons, Nil}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => ListCons(h(), t().toList)
  }
  override def toString: String = toList.toString
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    case _                   => Empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => {
      val hv = h()
      if (f(hv)) Cons(h, () => t().takeWhile(f)) else Empty
    }
    case _          => Empty
  }
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => {
      val hv = h()
      if (p(hv)) t().forAll(p) else false
    }
    case Empty      => true
  }
  def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => t().foldRight(f(h(),z))(f)
    case Empty      => z
  }
//  def takeWhile2(f: A=> Boolean): Stream[A] = foldRight(Empty[A])((h, v) => v match {
//    case Cons(h, t) => {
//      val hv = h()
//      if (f(hv)) t().foldRight(h, f)) else Empty
//    }
//    case _ => Empty
//  })

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head: A = hd
    lazy val tail: Stream[A] = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}

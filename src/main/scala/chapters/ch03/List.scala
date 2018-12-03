package chapters.ch03

sealed trait List[+A] {
  def map[B](f: A => B): List[B] = this match {
    case Cons(x, xs: List[A]) => Cons(f(x), xs.map(f))
    case Nil         => Nil
  }
  def tail: List[A] = this match {
    case Cons(a, t) => t
    case _          => Nil
  }
  def print(tag: String): Unit = {
    Predef.print(s"    $tag:")
    this.map(x => Predef.print("  " + x))
    Predef.println
  }
  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Cons(x, xs) => xs.foldRight(f(x, z))(f)
    case Nil         => z
  }
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    this match {
      case Cons(x, xs) => xs.foldLeft(f(z, x))(f)
      case Nil         => z
    }
  }
  def length = foldRight(0)((_, z) => z + 1)
  def append[B>:A](a: B): List[B] = this match {
    case Cons(x, xs) => Cons(x.asInstanceOf[A], xs.append(a))
    case Nil         => Cons(a, Nil)
  }
  def ++[B>:A](as: List[B]): List[B] = {
    as match {
      case Cons(x, xs) => append(x).++(xs)
      case Nil         => this
    }
  }
  def reverse: List[A] = this match {
    case Cons(x, xs) => xs.reverse append x
    case Nil         => Nil
  }
  def flatMap[B](f: A => List[B]): List[B] = this match {
    case Cons(x, xs) => f(x).++(xs.flatMap(f))
    case Nil         => Nil
  }
  def dropWhile(f: A => Boolean): List[A] = this match {
    case l @ Cons(a, t) => if (f(a)) t.dropWhile(f) else l
    case _ => Nil
  }
  def getFirst[AA >: A](f: A => Boolean): Option[A] = this match {
    case Cons(x, xs) if f(x) => Some(x)
    case Cons(x, xs)         => xs.getFirst(f)
    case Nil                 => None
  }
  def init: List[A] = {
    def _init(hl: List[A], tl: List[A]): List[A] = tl match {
      case Cons(tlh, tls @ Cons(_, _)) => _init(hl.append(tlh), tls)
      case _                           => hl
    }
    _init(Nil, this)
  }
  def filter(f: A => Boolean): List[A] = this match {
    case Cons(x, xs) => if (f(x)) Cons(x, xs.filter(f)) else xs.filter(f)
    case Nil         => Nil
  }
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)
  def apply(i: Int): Option[A] = this match {
    case _ if i < 0   => None
    case Nil if i > 0 => None
    case Cons(x, xs)  => if (i == 0) Some(x) else xs.apply(i - 1)
  }
}

final case object Nil extends List[Nothing]
final case class Cons[+A](head: A, override val tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def empty[A]: List[A] = Nil
}

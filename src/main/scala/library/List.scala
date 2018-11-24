package library

sealed trait List[+A] {
  def map[B](f: A => B): List[B] = this match {
    case Cons(x, xs: List[A]) => Cons(f(x), xs.map(f))
    case Nil         => Nil
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
  def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
      case Cons(x, xs) => xs.foldLeft(f(z, x))(f)
      case Nil         => z
    }
  def length = foldRight(0)((_, z) => z + 1)
  def append[A](a: A): List[A] = this match {
    case Cons(x, xs) => Cons(x.asInstanceOf[A], xs.append(a))
    case Nil         => Cons(a, Nil)
  }

}

final case object Nil extends List[Nothing]
final case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(as: List[Int]): Int = as.foldLeft(0)((x, y) => x + y)
  def product(as: List[Int]): Int = as.foldLeft(1)((x, y) => x * y)


  def product(doubles: List[Double]): Double = doubles match {
    case Cons(x, xs) => x * product(xs)
    case Nil         => 1.0
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Cons(x, xs) => foldRight(xs, f(x, z))(f)
      case Nil         => z
    }

  def foldRightMult(as: List[Double], z: Double, b: Double): Double =
    as match {
      case Cons(x, _) if x == b => b
      case Cons(x, xs)          => foldRightMult(xs, x * z, b)
      case Nil                  => z
    }

  def append[A](as: List[A], a: A):List[A] = as match {
    case Cons(x, xs) => Cons(x, append(xs, a))
    case Nil         => List(a)
  }

  def concatenate[A](asl: List[A], asr: List[A]): List[A] = asr match {
    case Cons(x, xs) => concatenate(append(asl, x), xs)
    case _           => asl
  }

  def reverse(as: List[Int]): List[Int] = as match {
    case Cons(x, xs) => append(reverse(xs), x)
    case Nil         => Nil
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
  def product3(ns: List[Double]) = foldRightMult(ns, 1.0, 0.0)
}
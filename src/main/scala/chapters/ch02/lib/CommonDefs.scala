package chapters.ch02.lib

object CommonDefs {
  def fib(n: Int): Int = {
    def _fib(n: Int, acc: Int): Int = {
      if (n < 2) acc else _fib(n - 1, acc * n)
    }
    _fib(n, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def _isSorted(as: Array[A], ordered: (A, A) => Boolean, pos: Int): Boolean = {
      if (pos >= as.length -1) true
      else if (!ordered(as(pos), as(pos+1))) false
      else _isSorted(as, ordered, pos+1)
    }
    _isSorted(as, ordered, 0)
  }

  def ascInt(a: Int, b: Int): Boolean = a <= b
  def descInt(a: Int, b: Int): Boolean = a >= b

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)
  def uncurry[A, B, C](f:A => B => C): (A, B) => C = f(_)(_)
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}

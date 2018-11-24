package chapters.ch02

object Ex_02_03 extends App{
  println("Ex_02_03")

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)

  val add = (a: Int, b: Int) => a + b
  val multiply = (a: Int, b: Int) => a * b
  val addCurry = curry(add)
  assert(curry(add)(5)(6) == add(5, 6))
  assert(curry(multiply)(5)(6) == multiply(5, 6))
}

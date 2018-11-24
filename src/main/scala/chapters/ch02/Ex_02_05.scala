package chapters.ch02

object Ex_02_05 extends App {
  println("Ex_02_05")

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  val f1 = (a: Int) => a + 5
  val f2 = (a: Int) => a * 3
  assert(compose(f1, f2)(5) == 20)
  assert(compose(f2, f1)(5) == 30)
}

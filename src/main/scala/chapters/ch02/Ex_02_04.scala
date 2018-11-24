package chapters.ch02

object Ex_02_04 extends App {
  println("Ex_02_04")

  def uncurry[A, B, C](f:A => B => C): (A, B) => C = f(_)(_)

  val addCurry = (a: Int) => (b: Int) => a + b
  val multiplyCurry = (a: Int) => (b: Int) => a * b
  assert(uncurry(addCurry)(5, 6) == addCurry(5)(6))
  assert(uncurry(multiplyCurry)(5, 6) == multiplyCurry(5)(6))
}

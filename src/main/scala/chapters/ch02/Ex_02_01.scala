package chapters.ch02

object Fib {
  private def _fib(n: Int, acc: Int): Int = {
    if (n < 2) acc else _fib(n - 1, acc * n)
  }
  def apply(n: Int) = _fib(n, 1)
}

object Ex_02_01 extends App {
  println("Ex_02_01")

  Seq(3, 2, 15).map( x => println(s"Fib($x) = ${Fib(x)}"))
}

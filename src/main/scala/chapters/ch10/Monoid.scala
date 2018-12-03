package chapters.ch10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
  def apply(a1: A, a2s: A*): A = a2s.toList.foldLeft(a1)(op)
}

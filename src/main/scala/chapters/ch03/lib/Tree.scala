package chapters.ch03.lib

sealed trait Tree[+A] {
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a)              => Leaf(f(a))
    case Branch(left, right)  => Branch(left.map(f), right.map(f))
  }
  def print: Unit = {
    def _print(tree: Tree[A], level: Int): Unit = tree match {
      case Leaf(x)             => println("src/main" * level + x.toString)
      case Branch(left, right) => {
        println("src/main" * level + "BRANCH")
        _print(left, level + 1)
        _print(right, level + 1)
      }
    }
    _print(this, 0)
  }
  def fold[A, B](at: Tree[A], z: B)(f: (A, B) => B): B = at match {
    case Leaf(a)             => f(a, z)
    case Branch(left, right) => {
      val leftFold = fold(left, z)(f)
      fold(right, leftFold)(f)
    }
  }
  def size: Int = fold(this, 0)((_, s) => s + 1)
}
final case class Leaf[A](value: A) extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
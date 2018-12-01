package chapters.ch04.lib

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a)    => Right(f(a))
    case e @ Left(_) => e
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a)    => f(a)
    case e @ Left(_) => e
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case a @ Right(_) => a
    case Left(_)      => b
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(av)    => b match {
      case Right(bv)    => Right(f(av, bv))
      case eb @ Left(_) => eb
    }
    case ea @ Left(_) => ea
  }
}
final case class Left[+E](value: E) extends Either[E, Nothing]
final case class Right[+A](value: A) extends Either[Nothing, A]
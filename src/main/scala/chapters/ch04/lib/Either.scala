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
  def getOrElse[B >: A](default: => B): B = this match {
    case Right(b) => b
    case _        => default
  }
  def get[B >: A]: B = this match {
    case Right(b)  => b
    case Left(_)   => throw new Exception("should not happen")
  }
  def exists: Boolean = this match {
    case Right(_) => true
    case Left(_)  => false
  }
  // TODO:
//  def filter[EE >: E](f: A => Boolean): Either[E, A] = {
//    this match {
//      case Right(a) if f(a) => Right(a)
//      case Right(a)         => Left(a)
//      case l @ Left(_)      => l
//    }
//  }
}
final case class Left[+E](value: E) extends Either[E, Nothing]
final case class Right[+A](value: A) extends Either[Nothing, A]

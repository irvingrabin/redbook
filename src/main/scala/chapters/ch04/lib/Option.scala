package chapters.ch04.lib

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None    => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None    => None
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None    => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(x) => this
    case None    => ob
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case _               => None
  }
}
final case class Some[A](value: A) extends Option[A]
final case object None extends Option[Nothing]

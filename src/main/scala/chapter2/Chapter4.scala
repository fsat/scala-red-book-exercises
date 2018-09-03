package chapter2

object Chapter4 {
  object Option {
    def apply[A](a: A): Option[A] =
      if (a == null) None else Some(a)
  }

  sealed trait Option[+A]
  case object None extends Option[Nothing]
  case class Some[A](a: A) extends Option[A]

  def map[A, B](o: Option[A])(f: A => B): Option[B] =
    o match {
      case Some(v) => Some(f(v))
      case _ => None
    }
}

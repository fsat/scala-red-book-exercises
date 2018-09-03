package chapter2

object Chapter4 {
  object Option {
    def apply[A](a: A): Option[A] =
      if (a == null) None else Some(a)

    def empty[A]: Option[A] = None
  }

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case Some(a) => Some(f(a))
        case _ => None
      }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this.map(a => f(a)).getOrElse(None)

    def getOrElse[B >: A](b: => B): B =
      this match {
        case Some(a) => a
        case _ => b
      }
  }

  case object None extends Option[Nothing]

  case class Some[A](a: A) extends Option[A]

}

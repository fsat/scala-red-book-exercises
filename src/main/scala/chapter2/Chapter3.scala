package chapter2

object Chapter3 {
  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def sum(ints: List[Int]): Int =
      ints match {
        case Nil => 0
        case Cons(head, tail) => head + sum(tail)
      }

    def tail[A](as: List[A]): List[A] =
      as match {
        case Nil => Nil
        case Cons(_, tail) => tail
      }
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
}

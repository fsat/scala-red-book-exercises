package chapter2

import scala.annotation.tailrec

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

    def setHead[A](a: A, list: List[A]): List[A] =
      Cons(a, list)

    def drop[A](list: List[A], n: Int): List[A] = {
      @tailrec
      def drop(idx: Int, result: List[A]): List[A] =
        if (idx == n)
          result
        else
          result match {
            case Nil => result
            case Cons(_, tail) => drop(idx + 1, tail)
          }

      drop(0, list)
    }
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
}

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

    def append[A](list: List[A], a: A): List[A] =
      list match {
        case Nil => List(a)
        case Cons(head, tail) => Cons(head, append(tail, a))
      }

    def dropWhile[A](list: List[A], cond: A => Boolean): List[A] = {
      @tailrec
      def iterateAndDrop(current: List[A], result: List[A]): List[A] =
        current match {
          case Nil => result
          case Cons(head, tail) =>
            val nextResult = if (cond(head)) result else append(result, head)
            iterateAndDrop(tail, nextResult)
        }

      iterateAndDrop(list, List())
    }

    def init[A](list: List[A]): List[A] = {
      @tailrec
      def iterateUntilSecondLast(current: List[A], result: List[A]): List[A] =
        current match {
          case Nil => result
          case Cons(_, Nil) => result
          case Cons(head, tail) =>
            iterateUntilSecondLast(tail, append(result, head))
        }

      iterateUntilSecondLast(list, List())
    }
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
}

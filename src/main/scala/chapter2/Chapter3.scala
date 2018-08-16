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

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def length[A](list: List[A]): Int = {
      def countLength(current: List[A], count: Int): Int =
        current match {
          case Nil => count
          case Cons(_, tail) => countLength(tail, count + 1)
        }

      countLength(list, 0)
    }

    def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def foldLeftToEnd(current: List[A], result: B): B =
        current match {
          case Nil => result
          case Cons(head, tail) => foldLeftToEnd(tail, f(result, head))
        }

      foldLeftToEnd(list, z)
    }

    def foldLeftLen[A](list: List[A]): Int = foldLeft(list, 0)((len, _) => len + 1)

    def foldLeftSum(list: List[Int]): Int = foldLeft(list, 0)(_ + _)

    def foldLeftProduct(list: List[Int]): Int = foldLeft(list, 1)(_ * _)

    def reverse[A](list: List[A]): List[A] =
      foldLeft(list, Nil: List[A])((result, elem) => setHead(elem, result))

    def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      val fReverse: (B, A) => B = (b, a) => f(a, b)

      foldLeft(reverse(as), z)(fReverse)
    }

    def foldLeftViaFoldRight[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
      val fReverse: (A, B) => B = (b, a) => f(a, b)
      foldRight(reverse(list), z)(fReverse)
    }

    def appendViaFoldRight[A](list: List[A], elem: A): List[A] =
      foldRight(list, List(elem))(Cons(_, _))

    def concat[A](a: List[List[A]]): List[A] =
      foldLeft[List[A], List[A]](a, Nil: List[A])((result, list) => foldLeft(list, result)(append))

    def map[A, B](list: List[A])(f: A => B): List[B] = {
      def mapToTheEnd(current: List[A], result: List[B]): List[B] =
        current match {
          case Nil => result
          case Cons(head, tail) => mapToTheEnd(tail, append(result, f(head)))
        }

      mapToTheEnd(list, Nil)
    }

    def plusOne(list: List[Int]): List[Int] = map(list)(_ + 1)

    def doubleToString(list: List[Double]): List[String] = map(list)(_.toString)

    def filter[A](list: List[A])(f: A => Boolean): List[A] =
      foldLeft(list, Nil: List[A])((result, elem) => if (f(elem)) append(result, elem) else result)

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      concat(
        foldLeft(list, Nil: List[List[B]]) {
          (result, elem) => append(result, f(elem))
        })

    def filterViaFlatMap[A](list: List[A])(f: A => Boolean): List[A] =
      flatMap(list)(elem => if (f(elem)) List(elem) else List())

    def zipWith[A, B, C](left: List[A], right: List[B])(f: (A, B) => C): List[C] = {
      @tailrec
      def zipToBothEnds(leftCurrent: List[A], rightCurrent: List[B], result: List[C]): List[C] =
        (leftCurrent, rightCurrent) match {
          case (Nil, _) => result
          case (_, Nil) => result
          case (Cons(leftHead, leftTail), Cons(rightHead, rightTail)) =>
            zipToBothEnds(leftTail, rightTail, append(result, f(leftHead, rightHead)))
        }

      zipToBothEnds(left, right, List())
    }

    def dropRight[A](list: List[A], size: Int): List[A] = {
      val listLength = length(list) - size

      @tailrec
      def collectToLimit(idx: Int, current: List[A], result: List[A]): List[A] =
        if (idx >= listLength)
          result
        else
          current match {
            case Nil => Nil
            case Cons(head, tail) => collectToLimit(idx + 1, tail, append(result, head))
          }

      collectToLimit(0, list, List())
    }

    def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
      val subLength = length(sub)

      @tailrec
      def hasSubsequenceFromLastToHead(current: List[A], built: List[A]): Boolean = {
        current match {
          case Nil => false
          case Cons(elem, tail) =>
            val appended = setHead(elem, built)
            val builtNext = if (length(appended) > subLength) dropRight(appended, 1) else appended

            val matchFound = builtNext == sub
            if (matchFound)
              true
            else
              hasSubsequenceFromLastToHead(tail, builtNext)
        }
      }

      hasSubsequenceFromLastToHead(reverse(list), List())
    }
  }

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
}

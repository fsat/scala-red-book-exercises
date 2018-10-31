package chapter2

import chapter2.Chapter5.Stream.cons

import scala.annotation.tailrec

object Chapter5 {
  sealed trait Stream[+A] {
    def toList(): List[A] = {
      @tailrec
      def recurse(ss: Stream[A], result: List[A]): List[A] =
        ss match {
          case Empty => result
          case Cons(hd, tl) => recurse(tl.apply(), result :+ hd.apply())
        }

      recurse(this, List.empty)
    }

    def append[B >: A](a: B): Stream[B] =
      this match {
        case Empty => cons(a, Empty)
        case Cons(hd, tl) => Cons(hd, () => tl.apply().append(a))
      }

    def take(n: Int): Stream[A] = {
      @tailrec
      def recurse(ss: Stream[A], count: Int, result: Stream[A]): Stream[A] = {
        if (count <= 0)
          result
        else
          ss match {
            case Empty => result
            case Cons(hd, tl) => recurse(tl.apply(), count - 1, result.append(hd.apply()))
          }
      }

      recurse(this, n, Empty)
    }

    def takeWhile(c: A => Boolean): Stream[A] = {
      @tailrec
      def recurse(ss: Stream[A], result: Stream[A]): Stream[A] =
        ss match {
          case Empty => result
          case Cons(hd, tl) =>
            val head = hd.apply()
            val next = if (c(head)) result.append(head) else result
            recurse(tl.apply(), next)
        }

      recurse(this, Empty)
    }

    def forAll(c: A => Boolean): Boolean = {
      @tailrec
      def recurse(ss: Stream[A]): Boolean =
        ss match {
          case Empty => true
          case Cons(hd, tl) =>
            val head = hd.apply()
            val ok = c(head)
            if (!ok)
              false
            else
              recurse(tl.apply())
        }
      recurse(this)
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}

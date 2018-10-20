package chapter2

import scala.annotation.tailrec

object Chapter5 {
  sealed trait Stream[+A]
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

    def toList[A](s: Stream[A]): List[A] = {
      @tailrec
      def recurse(ss: Stream[A], result: List[A]): List[A] =
        ss match {
          case Empty => result
          case Cons(hd, tl) => recurse(tl.apply(), result :+ hd.apply())
        }

      recurse(s, List.empty)
    }

    def append[A](s: Stream[A], a: A): Stream[A] =
      s match {
        case Empty => cons(a, Empty)
        case Cons(hd, tl) => cons(hd.apply(), append(tl.apply(), a))
      }

    def take[A](s: Stream[A], n: Int): Stream[A] = {
      @tailrec
      def recurse(ss: Stream[A], count: Int, result: Stream[A]): Stream[A] = {
        if (count <= 0)
          result
        else
          ss match {
            case Empty => result
            case Cons(hd, tl) => recurse(tl.apply(), count - 1, append(result, hd.apply()))
          }
      }

      recurse(s, n, Empty)
    }
  }
}

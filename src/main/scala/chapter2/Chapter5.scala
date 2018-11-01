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

    def append[B >: A](a: => B): Stream[B] =
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

    def takeWhile(c: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Stream.empty[A]) { (elem, result) =>
        if (c(elem)) {
          Stream.cons(elem, result)
        } else
          result
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

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
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

    def constant[A](a: A): Stream[A] =
      unfold(a)(_ => Some(a, a))

    def from(a: Int): Stream[Int] =
      unfold(a)(a => Some(a, a + 1))

    def fib(): Stream[Int] =
      unfold((0, 1)) { v =>
        val (a, b) = v
        Some(a, (b, a + b))
      }
    //    {
    //      def fib(a: Int, b: Int): Stream[Int] =
    //        Stream.cons(a, fib(b, a + b))
    //
    //      fib(0, 1)
    //    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, b)) => Stream.cons(a, unfold(b)(f))
        case _ => Stream.empty
      }
    }
  }
}

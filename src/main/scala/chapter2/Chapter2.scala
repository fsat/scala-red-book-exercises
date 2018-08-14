package chapter2

import scala.annotation.tailrec

object Chapter2 {
  def fib(n: Int): Int = {
    @tailrec
    def fib(x: Int, sum: Int, sumPrev: Int): Int = {
      if (x == n)
        sum
      else {
        val nextSum = if (x <= 1) 1 else sum + sumPrev
        fib(x + 1, nextSum, sum)
      }
    }

    fib(0, 0, 0)
  }

  def isOrdered[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def isOrdered(elem: A, next: Array[A], result: Boolean): Boolean =
      if (!result || next.isEmpty)
        // If there's a false, then stop computing since result will be false.
        result
      else
        isOrdered(next.head, next.tail, result && ordered(elem, next.head))

    if (arr.isEmpty)
      true
    else
      isOrdered(arr.head, arr.tail, result = true)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}

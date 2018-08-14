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
}

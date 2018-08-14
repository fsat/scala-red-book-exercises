package chapter2

import base.UnitTestLike

class Chapter2Spec extends UnitTestLike {
  describe("fibonacci") {
    Seq(
      0 -> 0,
      1 -> 1,
      2 -> 1,
      3 -> 2,
      5 -> 5,
      10 -> 55,
      11 -> 89,
      20 -> 6765).foreach {
        case (input, result) =>
          it(s"fib($input) == $result") {
            Chapter2.fib(input) shouldBe result
          }
      }
  }

  describe("isOrdered") {

    def lessEqualThan(a: Int, b: Int): Boolean = a <= b

    Seq(
      Array.empty[Int] -> true,
      Array(1) -> true,
      Array(1, 2) -> true,
      Array(2, 1) -> false,
      Array(1, 2, 3, 4, 5) -> true,
      Array(1, 3, 2, 3, 4, 5) -> false).foreach {
        case (input, result) =>
          it(s"${input.toSeq} returns $result") {
            Chapter2.isOrdered(input, lessEqualThan) shouldBe result
          }
      }

  }
}

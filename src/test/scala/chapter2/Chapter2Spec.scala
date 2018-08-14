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
}

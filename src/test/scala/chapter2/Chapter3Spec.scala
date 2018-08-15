package chapter2

import base.UnitTestLike

class Chapter3Spec extends UnitTestLike {
  import Chapter3.{ List, Cons, Nil }
  import Chapter3.List._

  describe("pattern match list") {
    it("matches expected value") {
      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }

      x shouldBe 3
    }
  }

  describe("tail") {
    it("returns remainder of the list") {
      tail(List(1, 2, 3, 4, 5)) shouldBe List(2, 3, 4, 5)
    }

    it("returns nil for empty list") {
      tail(List()) shouldBe Nil
    }
  }

  describe("setHead") {
    it("prepends the element") {
      setHead(0, List(1, 2, 3, 4, 5)) shouldBe List(0, 1, 2, 3, 4, 5)
    }

    it("creates a list from an empty list") {
      setHead(0, List()) shouldBe List(0)
    }
  }

  describe("drop") {
    Seq(
      ("nil", Nil, 1, Nil),
      ("drop all elements", List(1), 1, Nil),
      ("drop < size of the list", List(1, 2, 3), 1, List(2, 3)),
      ("drop > size of the list", List(1, 2, 3), 5, Nil)).foreach {
        case (scenario, input, size, expectedResult) =>
          it(s"handles $scenario") {
            drop(input, size) shouldBe expectedResult
          }
      }
  }
}

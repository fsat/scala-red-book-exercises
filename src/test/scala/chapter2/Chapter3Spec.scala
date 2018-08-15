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
}

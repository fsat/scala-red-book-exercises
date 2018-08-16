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

  describe("dropWhile") {
    it("drops all element matching condition") {
      dropWhile[Int](List(1, 2, 3, 4, 5), _ % 2 == 0) shouldBe List(1, 3, 5)
    }
  }

  describe("init") {
    Seq(
      ("drops the last element", List(1, 2, 3, 4, 5), List(1, 2, 3, 4)),
      ("handles nil", Nil, Nil)).foreach {
        case (scenario, input, expectedResult) =>
          it(scenario) {
            init(input) shouldBe expectedResult
          }
      }
  }

  describe("foldRight") {
    it("handles Nil and Cons") {
      foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
    }
  }

  describe("length") {
    it("returns the length of the list") {
      List.length(List(1, 2, 3)) shouldBe 3
    }
  }

  describe("foldLeft") {
    it("folds correctly") {
      foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
    }

    it("handles Nil and Cons") {
      foldLeft(List(1, 2, 3), Nil: List[Int])(append) shouldBe List(1, 2, 3)
    }
  }

  describe("foldLeftLen") {
    it("returns the length of the list") {
      foldLeftLen(List(1, 2, 3)) shouldBe 3
    }
  }

  describe("foldLeftSum") {
    it("returns the sum of the list") {
      foldLeftSum(List(1, 2, 3)) shouldBe 6
    }
  }

  describe("foldLeftProduct") {
    it("returns the product of the list") {
      foldLeftProduct(List(1, 2, 4)) shouldBe 8
    }
  }

  describe("reverse") {
    it("reverses the list") {
      reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    }
  }
}

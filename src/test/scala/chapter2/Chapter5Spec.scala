package chapter2

import base.UnitTestLike

class Chapter5Spec extends UnitTestLike {
  import Chapter5._

  describe("toList") {
    it("converts the stream to list") {
      Stream(1, 2, 3, 4).toList() shouldBe List(1, 2, 3, 4)
    }
  }

  describe("take") {
    it("takes n element from the head of stream") {
      Stream(1, 2, 3, 4).take(2).toList() shouldBe List(1, 2)
    }

    it("handles n element larger than the stream length") {
      Stream(1, 2, 3, 4).take(50).toList() shouldBe List(1, 2, 3, 4)
    }
  }

  describe("takeWhile") {
    it("takes element which matches predicate") {
      Stream(1, 2, 3, 4).takeWhile(_ % 2 == 0).toList() shouldBe List(2, 4)
    }
  }
}

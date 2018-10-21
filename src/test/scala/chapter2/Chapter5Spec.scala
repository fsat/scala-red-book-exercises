package chapter2

import base.UnitTestLike

class Chapter5Spec extends UnitTestLike {
  import Chapter5._

  describe("toList") {
    it("converts the stream to list") {
      Stream.toList(Stream(1, 2, 3, 4)) shouldBe List(1, 2, 3, 4)
    }
  }

  describe("take") {
    it("takes n element from the head of stream") {
      val x = Stream.take(Stream(1, 2, 3, 4), n = 2)
      Stream.toList(x) shouldBe List(1, 2)
    }

    it("handles n element larger than the stream length") {
      val x = Stream.take(Stream(1, 2, 3, 4), n = 50)
      Stream.toList(x) shouldBe List(1, 2, 3, 4)
    }
  }

  describe("takeWhile") {
    it("takes element which matches predicate") {
      val x = Stream.takeWhile(Stream(1, 2, 3, 4))(_ % 2 == 0)
      Stream.toList(x) shouldBe List(2, 4)
    }
  }
}

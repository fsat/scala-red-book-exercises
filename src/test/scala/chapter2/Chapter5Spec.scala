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

  describe("forAll") {
    it("returns true only if all elements are true") {
      val s = Stream(1, 2, 3, 4)
      s.forAll(_ > 0) shouldBe true
      s.forAll(_ <= 2) shouldBe false
    }
  }

  // TODO: 5.6 and 5.7

  describe("constant") {
    it("returns stream of constant value") {
      Stream.constant("hey").take(100).toList() shouldBe (0 until 100).map(_ => "hey")
    }
  }

  describe("from") {
    it("returns stream of constantly incrementing integer") {
      Stream.from(0).take(100).toList() shouldBe (0 until 100)
      Stream.from(1000).take(100).toList() shouldBe (0 until 100).map(_ + 1000)
    }
  }

  describe("fib") {
    it("returns fibonacci sequence") {
      Stream.fib().take(7).toList() shouldBe Seq(0, 1, 1, 2, 3, 5, 8)
    }
  }

  describe("unfold") {
    it("returns the stream until none is returned") {
      val s = Stream.unfold(0) { number =>
        if (number <= 3)
          Some(number, number + 1)
        else
          None
      }

      s.toList() shouldBe Seq(0, 1, 2, 3)
    }
  }
}

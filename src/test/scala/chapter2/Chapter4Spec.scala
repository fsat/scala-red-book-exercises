package chapter2

import base.UnitTestLike

class Chapter4Spec extends UnitTestLike {
  import Chapter4._

  describe("apply") {
    it("returns some value given a value") {
      Option(1) shouldBe Some(1)
    }

    it("returns none given null") {
      Option(null) shouldBe None
    }
  }

  describe("map") {
    it("returns result of mapped function if present") {
      Option(1).map(_.toLong + 10L) shouldBe Option(11L)
    }

    it("returns nothing otherwise") {
      Option.empty[Int].map(_.toLong + 10L) shouldBe None
    }
  }

  describe("getOrElse") {
    it("returns the value inside the option") {
      Option(1).getOrElse("hey") shouldBe 1
    }

    it("returns default value") {
      Option.empty[Int].getOrElse("hey") shouldBe "hey"
    }
  }
}

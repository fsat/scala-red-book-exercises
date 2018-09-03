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
      map(Option(1))(_.toLong + 10L) shouldBe Option(11L)
    }

    it("returns nothing otherwise") {
      map[Int, Long](None)(_.toLong + 10L) shouldBe None
    }
  }
}

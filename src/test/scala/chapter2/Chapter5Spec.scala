package chapter2

import base.UnitTestLike

class Chapter5Spec extends UnitTestLike {
  import Chapter5._

  describe("toList") {
    it("converts the stream to list") {
      Stream.toList(Stream(1, 2, 3, 4)) shouldBe List(1, 2, 3, 4)
    }
  }
}

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
    describe("Some") {
      it("returns result of mapped function if present") {
        Option(1).map(_.toLong + 10L) shouldBe Option(11L)
      }

      it("returns nothing otherwise") {
        Option.empty[Int].map(_.toLong + 10L) shouldBe None
      }
    }

    describe("None") {
      it("returns none regardless") {
        Option.empty[Int].map(_ + 1) shouldBe None
      }
    }
  }

  describe("flatMap") {
    describe("Some") {
      it("returns result of mapped function if present") {
        Option(1).flatMap(v => Some(v.toLong + 10L)) shouldBe Option(11L)
      }

      it("returns none if result of mapped function is none") {
        Option(1).flatMap[Long](_ => None) shouldBe None
      }
    }

    describe("None") {
      it("returns none given mapped function which returns some value") {
        Option.empty[Int].flatMap(v => Some(v.toLong + 10L)) shouldBe None
      }

      it("returns none given mapped function which returns none") {
        Option.empty[Int].flatMap(_ => None) shouldBe None
      }
    }
  }

  describe("getOrElse") {
    it("returns the value inside the option") {
      Option(1).getOrElse(fail("should not be called")) shouldBe 1
    }

    it("returns default value") {
      Option.empty[Int].getOrElse("hey") shouldBe "hey"
    }
  }

  describe("orElse") {
    it("returns the original option") {
      Option(1).orElse(Option("fail")) shouldBe Option(1)
    }

    it("returns the fallback option") {
      Option.empty[Int].orElse(Option("hey")) shouldBe Option("hey")
    }
  }

  describe("filter") {
    describe("Some") {
      it("returns some value if filter returns true") {
        Option(1).filter(_ == 1) shouldBe Option(1)
      }

      it("returns none if filter returns false") {
        Option(1).filter(_ != 1) shouldBe None
      }
    }

    describe("None") {
      it("returns none regardless of filter ") {
        Option.empty[Int].filter(_ => true) shouldBe None
      }
    }
  }
}

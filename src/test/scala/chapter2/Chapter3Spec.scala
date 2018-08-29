package chapter2

import base.UnitTestLike

class Chapter3Spec extends UnitTestLike {
  describe("list") {
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

    describe("foldRightViaFoldLeft") {
      it("handles Nil and Cons") {
        foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
      }
    }

    describe("foldLeftViaFoldRight") {
      it("handles Nil and Cons") {
        foldLeftViaFoldRight(List(1, 2, 3), Nil: List[Int])(append) shouldBe List(1, 2, 3)
      }
    }

    describe("appendViaFoldRight") {
      it("appends the element to the end of list") {
        appendViaFoldRight(List(1, 2, 3), 4) shouldBe List(1, 2, 3, 4)
      }
    }

    describe("concat") {
      it("concats list of lists into a single list") {
        concat(List(List(1), List(2, 3), List(4, 5))) shouldBe List(1, 2, 3, 4, 5)
      }
    }

    describe("map") {
      it("transforms the list") {
        map(List(1, 2, 3))(v => s"Hey $v") shouldBe List("Hey 1", "Hey 2", "Hey 3")
      }
    }

    describe("plusOne") {
      it("adds one to the list element") {
        plusOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
      }
    }

    describe("doubleToString") {
      it("converts list element to string") {
        doubleToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
      }
    }

    describe("filter") {
      it("filters only wanted elements") {
        filter(List(1, 2, 3, 4))(_ % 2 != 0) shouldBe List(1, 3)
      }
    }

    describe("flatMap") {
      it("flat maps over a list") {
        flatMap(List(1, 2, 3))(v => List(v, v)) shouldBe List(1, 1, 2, 2, 3, 3)
      }
    }

    describe("filterViaFlatMap") {
      it("filters only wanted elements") {
        filterViaFlatMap(List(1, 2, 3, 4))(_ % 2 != 0) shouldBe List(1, 3)
      }
    }

    describe("zipWith") {
      def plus(a: Int, b: Int): Int = a + b

      Seq(
        ("left nil", List[Int](), List(10, 11, 12), List[Int]()),
        ("right nil", List(100, 200, 300), List[Int](), List[Int]()),
        ("balanced", List(100, 200, 300), List(10, 11, 12), List(110, 211, 312)),
        ("more left", List(100, 200, 300, -1), List(10, 11, 12), List(110, 211, 312)),
        ("more right", List(100, 200, 300), List(10, 11, 12, -1), List(110, 211, 312))).foreach {
          case (scenario, left, right, expectedResult) =>
            it(s"handles $scenario") {
              zipWith(left, right)(plus) shouldBe expectedResult
            }
        }
    }

    describe("dropRight") {
      it("drops elements from the right") {
        dropRight(List(1, 2, 3, 4, 5), 2) shouldBe List(1, 2, 3)
      }
    }

    describe("hasSubsequence") {
      Seq(
        ("subsequent is present - 1", List(1, 2, 3, 4), List(1, 2), true),
        ("subsequent is present - 2", List(1, 2, 3, 4), List(2, 3), true),
        ("subsequent is present - 3", List(1, 2, 3, 4), List(3, 4), true),
        ("subsequent is not present", List(1, 2, 3, 4), List(1, 3), false)).foreach {
          case (scenario, list, subList, expectedResult) =>
            it(s"returns $expectedResult when $scenario") {
              hasSubsequence(list, subList) shouldBe expectedResult
            }
        }
    }
  }

  describe("tree") {
    import Chapter3.{ Tree, Branch, Leaf }
    import Chapter3.Tree._

    describe("size") {
      Seq(
        ("leaf", Leaf(1), 1),
        ("simple tree", Branch(Leaf(1), Leaf(3)), 3),
        ("complex tree",
          Branch(
            Branch(
              Branch(
                Leaf(1),
                Leaf(2)),
              Leaf(3)),
            Branch(
              Leaf(4),
              Leaf(5))), 9)).foreach {
          case (scenario, input, expectedResult) =>
            it(scenario) {
              Tree.size(input) shouldBe expectedResult
            }
        }
    }

    describe("maximum") {
      Seq(
        ("leaf", Leaf(1), 1),
        ("simple tree", Branch(Leaf(1), Leaf(3)), 3),
        ("complex tree",
          Branch(
            Branch(
              Branch(
                Leaf(1),
                Leaf(2002)),
              Leaf(37)),
            Branch(
              Leaf(41),
              Branch(
                Leaf(2),
                Leaf(3)))), 2002)).foreach {
          case (scenario, input, expectedResult) =>
            it(scenario) {
              Tree.maximum(input) shouldBe expectedResult
            }
        }
    }

  }

}

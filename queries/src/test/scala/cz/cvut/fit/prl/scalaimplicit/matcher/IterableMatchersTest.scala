package cz.cvut.fit.prl.scalaimplicit.matcher

import org.scalatest.{FunSuite, Matchers => ScalaTestMatchers}

class IterableMatchersTest extends FunSuite with ScalaTestMatchers with ScalaTestMatchingSupport with Matchers {

  test("size") {
    size(2).description shouldBe "size that is 2"
    size(is(2)).description shouldBe "size that is 2"
    size(2).negativeDescription shouldBe "size that is not 2"
    size(is(2)).negativeDescription shouldBe "size that is not 2"

    (Seq(1, 2) matches size(2)) should matched("size is 2")
    (Seq(1, 2) matches size(is(2))) should matched("size is 2")
    (Seq(1, 2) matches size(gt(1))) should matched("size is > 1")

    (Seq(1, 2) matches size(3)) should mismatched("size(2) is not 3")
    (Seq(1, 2) matches size(is(3))) should mismatched("size(2) is not 3")
    (Seq(1, 2) matches size(gt(3))) should mismatched("size(2) is <= 3")
  }

  test("is empty") {

  }

  test("contains") {

  }

  test("inOrderOnly") {
    (Seq(1, 2, 3) matches inOrderOnly(is(4))) should mismatched("List(1, 2, 3) is missing in order {0: (1) is not 4, 1: 2 should be empty, 2: 3 should be empty}")
    (Seq(1) matches inOrderOnly(is(1), is(2), is(3))) should mismatched("List(1) is missing in order {1: empty value is not 2, 2: empty value is not 3}")
  }

  test("allOf") {
    val p: Matcher[Iterable[Int]] = allOf(1, 2)

    p.description shouldBe "contains [1, 2]"
    p.negativeDescription shouldBe "does not contain [1, 2]"

    (Seq(1, 2, 3) matches allOf(1, 2)) should matched("contains [1, 2]")
    (Seq(1, 2, 3) matches allOf(3, 4, 5)) should mismatched("List(1, 2, 3) is missing [4, 5]")
  }

  test("contains in order") {

  }

}

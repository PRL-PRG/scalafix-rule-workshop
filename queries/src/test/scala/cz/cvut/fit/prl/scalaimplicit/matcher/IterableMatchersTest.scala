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

    (Seq(1, 2) matches size(3)) should mismatched("size (2) is not 3")
    (Seq(1, 2) matches size(is(3))) should mismatched("size (2) is not 3")
    (Seq(1, 2) matches size(gt(3))) should mismatched("size (2) is <= 3")
  }

  test("is empty") {
    isEmpty.description shouldBe "is empty"
    isEmpty.negativeDescription shouldBe "is not empty"

    (Seq(1) matches isEmpty) should mismatched("List(1) is not empty")
    (Seq() matches isEmpty) should matched("is empty")
    (Nil matches isEmpty) should matched("is empty")
    (List() matches isEmpty) should matched("is empty")
    (Map(1 -> 2, 2 -> 3) matches isEmpty) should mismatched("Map(1 -> 2, 2 -> 3) is not empty")
  }

  test("contains") {
    val p1 = contains(is(1), gt(1))

    p1.description shouldBe "contains [is 1, is > 1]"
    p1.negativeDescription shouldBe "does not contain [is 1, is > 1]"

    (Seq(1, 2) matches p1) should matched("contains [is 1, is > 1]")
    (Seq(1, 2) matches contains(is(1), gt(2))) should mismatched("List(1, 2) is missing [is > 2]")
    (Seq(1, 2) matches contains(is(3), gt(2))) should mismatched("List(1, 2) is missing [is 3, is > 2]")
    (Seq(1, 2) matches contains(and(gt(1), gt(2)))) should mismatched("List(1, 2) is missing [(is > 1) && (is > 2)]")
  }

  test("inOrderOnly") {
    val p1 = inOrderOnly(is(1), is(2))

    p1.description shouldBe "contains in order only [is 1, is 2]"
    p1.negativeDescription shouldBe "does not contain in order [is 1, is 2]"

    (Seq(1, 2, 3) matches inOrderOnly(is(4))) should mismatched("List(1, 2, 3) not matching on indices {0: (1) is not 4, 1: 2 should be empty, 2: 3 should be empty}")
    (Seq(1) matches inOrderOnly(is(1), is(2), is(3))) should mismatched("List(1) not matching on indices {1: empty value is not 2, 2: empty value is not 3}")
  }

  test("allOf") {
    val p: Matcher[Iterable[Int]] = allOf(1, 2)

    p.description shouldBe "contains [1, 2]"
    p.negativeDescription shouldBe "does not contain [1, 2]"

    (Seq(1, 2, 3) matches allOf(1, 2)) should matched("contains [1, 2]")
    (Seq(1, 2, 3) matches allOf(3, 4, 5)) should mismatched("List(1, 2, 3) is missing [4, 5]")

    (Map(1 -> 2) matches allOf(1 -> 2)) should matched("contains [(1,2)]")
    (Map(1 -> 2) matches allOf(1 -> 3)) should mismatched("Map(1 -> 2) is missing [(1,3)]")
  }

  test("combinations of size, contains and allof to test polymorphism") {
    import TestClasses._

    (lx matches and(contains(is(x1)), contains(is(y1)))) should matched("(contains [is X(1)]) && (contains [is Y(1,1)])")
    (lx matches contains(is(x1), is(y1))) should matched("contains [is X(1), is Y(1,1)]")
    (lx matches and(allOf(x1), allOf(y1))) should matched("(contains [X(1)]) && (contains [Y(1,1)])")
    (lx matches allOf(x1, y1)) should matched("contains [X(1), Y(1,1)]")

    (lx matches and(size(1), allOf(y1))) should mismatched("size (2) is not 1")

    (lx matches and(contains(is(x1)), contains(is(y1)))) should matched
    (lx matches and(contains(is(y1)), contains(is(x1)))) should matched

    (lx matches and(allOf(x1), size(is(2)))) should matched
    (lx matches allOf(y1)) should matched

    (lx matches and(allOf(y1), allOf(x1))) should matched
    (lx matches and(allOf(x1), allOf(y1))) should matched
    (lx matches allOf(y1, x1, 1)) should mismatched

    (lx matches is(lx)) should matched
    (lx matches is(ly)) should mismatched
  }

}

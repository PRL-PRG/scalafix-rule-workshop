package cz.cvut.fit.prl.scalaimplicit.matcher

import org.scalatest.{FunSuite, Matchers => ScalaTestMatchers}

class PropertyMatchersTest extends FunSuite with ScalaTestMatchers with ScalaTestMatchingSupport with Matchers {

  test("property matcher") {
    case class X(p1: Int, p2: Int)

    val p1 = PropertyMatcher[X, Int]("p1", _.p1, gt(1))
    val p2 = PropertyMatcher[X, Int]("p2", _.p2, lt(1))
    val p1p2 = and(p1, p2)

    p1.description shouldBe "p1 that is > 1"
    p1.negativeDescription shouldBe "p1 that is <= 1"

    p2.description shouldBe "p2 that is < 1"
    p2.negativeDescription shouldBe "p2 that is >= 1"

    p1p2.description shouldBe "(p1 that is > 1) && (p2 that is < 1)"
    p1p2.negativeDescription shouldBe "(p1 that is <= 1) || (p2 that is >= 1)"

    (X(2, 1) matches p1) should matched("p1 is > 1")
    (X(1, 1) matches p1) should mismatched("p1(1) is <= 1")

    (X(1, 0) matches p1p2) should mismatched("p1(1) is <= 1")
    (X(2, 1) matches p1p2) should mismatched("p2(1) is >= 1")
    (X(1, 1) matches p1p2) should mismatched("(p1(1) is <= 1) && (p2(1) is >= 1)")
  }

  test("boolean matcher") {
    case class X(p1: Boolean)

    val p1 = BooleanPropertyMatcher[X]("p1", _.p1)

    p1.description shouldBe "is p1"
    p1.negativeDescription shouldBe "is not p1"

    (X(true) matches p1) should matched("is p1")
    (X(false) matches p1) should mismatched("X(false) is not p1")
  }

  test("option matcher") {
    case class X(p1: Option[Int])

    val p1 = OptionPropertyMatcher[X, Int]("p1", _.p1, gt(1))

    p1.description shouldBe "p1 value that is > 1"
    p1.negativeDescription shouldBe "p1 value that is <= 1"

    (X(Some(2)) matches p1) should matched("p1 value is > 1")
    (X(Some(1)) matches p1) should mismatched("p1 value(1) is <= 1")
    (X(None) matches p1) should mismatched("p1 is None")
  }
}

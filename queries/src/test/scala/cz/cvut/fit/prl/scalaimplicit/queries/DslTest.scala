import org.scalatest.{FunSuite, Matchers}
import cats.syntax.semigroup._

class DslTest extends FunSuite with Matchers {

  import cz.cvut.fit.prl.scalaimplicit.queries
  import cz.cvut.fit.prl.scalaimplicit.queries.Matchers._
  import cz.cvut.fit.prl.scalaimplicit.queries.Matchers.implicits._

  implicit class TestHelper[A](that: Matcher[A]) {
    def matches(v: A): Unit = if (!that(v).matches) {
      fail(that.describeMismatch(v).get)
    }

    def notMatches(v: A): Unit = if (that(v).matches) {
      fail(that.describeMatch(v).get)
    }
  }

  test("is matcher matches an instance of ") {
    is(2) matches 2

    is(2).describeMatch(2) should contain ("2 == 2")
    is(2).describeMismatch(1) should contain ("1 != 2")
  }

  test("combine matchers") {
    val c = is(2) |+| is(1)
    c.description shouldBe "== 2\n== 1"
    c.describeMismatch(1) should contain ("1 != 2")
    c.describeMismatch(2) should contain ("2 != 1")
    c.describeMatch(1) shouldBe empty
    c.describeMatch(2) shouldBe empty
  }

  test("not") {
    !is(2) matches 1
  }

  test("in") {
    in(1, 2, 3) matches 2
    in(1, 2, 3).describeMatch(2) should contain ("2 in {1,2,3}")
    !in(1, 2, 3) matches 4
  }

  test("ordering") {
    gt(2) matches 3
  }

  test(" fasdf ") {
    val x = 1 :: 2 :: Nil

    x.matches(queries.Matchers.size(!in(1,2)))

    val m: Matcher[List[Int]] = queries.Matchers.size(!in(1,2))

    m.describeMismatch(x) should contain ("size 2 in {1,2}")
    m.describeMatch(3 :: x) should contain ("size 3 not in {1,2}")

    class A {
      def size = 4
    }

    val a = new A
    val am: Matcher[A] = queries.Matchers.size(!in(1,2))
    am.describeMatch(a) should contain ("size 4 not in {1,2}")

  }

  test("property matcher - size") {
    val x = 1 :: 2 :: Nil
    val m: Matcher[List[Int]] = queries.Matchers.size(is(2))

    m matches x
    m.describeMatch(x) should contain ("size `2' == `2'")
    m.describeMatch(3 :: x) shouldBe empty
    m.describeMismatch(3 :: x) should contain ("size `3' != `2'")
  }
}

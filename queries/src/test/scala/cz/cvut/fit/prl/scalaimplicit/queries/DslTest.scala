import org.scalatest.{FunSuite, Matchers}
import cats.syntax.semigroup._

class DslTest extends FunSuite with Matchers {

  import cz.cvut.fit.prl.scalaimplicit.queries.Matchers._

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

    is(2).describeMatch(2) shouldBe Some("`2' == `2'")
    is(2).describeMismatch(1) shouldBe Some("`1' != `2'")
  }

  test("combine matchers") {
    val c = is(2) |+| is(1)
    c.description shouldBe "== `2'\n== `1'"
    c.describeMismatch(1) shouldBe Some("`1' != `2'")
    c.describeMismatch(2) shouldBe Some("`2' != `1'")
    c.describeMatch(1) shouldBe None
    c.describeMatch(2) shouldBe None
  }

  test("not") {
    !is(2) matches 1
  }
}

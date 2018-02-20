import org.scalatest.{FunSuite, Matchers => STMatchers}
import cats.syntax.semigroup._
import org.scalatest.matchers.{MatchResult => STMatchResult, Matcher => STMatcher}

class DslTest extends FunSuite with STMatchers {

  import cz.cvut.fit.prl.scalaimplicit.queries._
  import cz.cvut.fit.prl.scalaimplicit.queries.Matchers._
  import cz.cvut.fit.prl.scalaimplicit.queries.Matchers.implicits._

  trait MatcherTesting {
    private def test(matches: Boolean, reason: Option[String]) = new STMatcher[MatchResult[_]] {
      def apply(left: MatchResult[_]): STMatchResult = {
        if (left.matches == matches) {
          val reasonCheck =
            reason.map(STMatchers.be(_).apply(left.reason)).getOrElse(STMatchResult(true, "", ""))

          STMatchResult(
            matches = reasonCheck.matches,
            s"$left ${if (matches) "" else "mis"}matched but with a different reason: ${reasonCheck.failureMessage}",
            s"$left ${if (matches) "" else "mis"}matched ${left.reason}"
          )
        } else {
          STMatchResult(
            matches = false,
            s"$left ${if (matches) "" else "mis"}matched ${left.reason}",
            s"$left ${if (!matches) "" else "mis"}matched ${left.reason}"
          )
        }
      }
    }

    def matched(reason: String): STMatcher[MatchResult[_]] = test(true, Some(reason))

    def matched: STMatcher[MatchResult[_]] = test(true, None)

    def mismatched(reason: String): STMatcher[MatchResult[_]] = test(false, Some(reason))

    def mismatched: STMatcher[MatchResult[_]] = test(false, None)
  }

  object MatcherTesting extends MatcherTesting

  import MatcherTesting._

  test("is matcher matches an instance of ") {
    (2 matches is(2)) should matched
    (1 matches is(2)) should mismatched
  }

  test("combine matchers using default combinator AND") {
    val c = is(2) |+| is(1)
    c.description shouldBe "== 2 && == 1"
    c.negativeDescription shouldBe "!= 2 || != 1"

    (2 matches c) should mismatched("2 != 1")
    (1 matches c) should mismatched("1 != 2")
  }

  test("negate matcher") {
    (1 matches !is(2)) should matched("1 != 2")
  }

  test("negate negated matcher") {
    //noinspection DoubleNegationScala
    (1 matches !(!is(1))) should matched("1 == 1")
  }

  test("combine matchers using OR") {
    val c = is(1) || is(2) || is(3)
    c.description shouldBe "== 1 || == 2 || == 3"
    c.negativeDescription shouldBe "!= 1 && != 2 && != 3"

    (1 matches c) should matched("1 == 1")
    (2 matches c) should matched("2 == 2")
    (3 matches c) should matched("3 == 3")
    (4 matches c) should mismatched("4 != 1 && 4 != 2 && 4 != 3")
  }

  //  test("in") {
  //    in(1, 2, 3) matches 2
  //    in(1, 2, 3).describeMatch(2) should contain ("2 in {1,2,3}")
  //    !in(1, 2, 3) matches 4
  //  }
  //
  //  test("ordering") {
  //    gt(2) matches 3
  //  }
  //
  //  test("value format") {
  //    class A
  //    implicit val avf = new ValueFormat[A] { def format(x:A) = "My A" }
  //    val a = new A
  //
  //    is(a).describeMatch(a) should contain ("My A")
  //  }
  //
  //  test(" fasdf ") {
  //    val x = 1 :: 2 :: Nil
  //
  //    x.matches(queries.Matchers.size(!in(1,2)))
  //
  //    val m: Matcher[List[Int]] = queries.Matchers.size(!in(1,2))
  //
  //    m.describeMismatch(x) should contain ("size 2 in {1,2}")
  //    m.describeMatch(3 :: x) should contain ("size 3 not in {1,2}")
  //
  //    class A {
  //      def size = 4
  //    }
  //
  //    val a = new A
  //    val am: Matcher[A] = queries.Matchers.size(!in(1,2))
  //    am.describeMatch(a) should contain ("size 4 not in {1,2}")
  //
  //  }
  //
  //  test("property matcher - size") {
  //    val x = 1 :: 2 :: Nil
  //    val m: Matcher[List[Int]] = queries.Matchers.size(is(2))
  //
  //    m matches x
  //    m.describeMatch(x) should contain ("size `2' == `2'")
  //    m.describeMatch(3 :: x) shouldBe empty
  //    m.describeMismatch(3 :: x) should contain ("size `3' != `2'")
  //  }

  test("collection matching") {
    val q = is(2) || is(4)
    val col = Seq(1, 2, 3, 4)

   (col query q) should contain theSameElementsInOrderAs Seq(Mismatch(1, q), Match(2, q), Mismatch(3, q), Match(4, q))
   (col query q) map (_.toOption) should contain theSameElementsInOrderAs Seq(None, Some(2), None, Some(4))
   (col query q) map (_.toEither) should contain theSameElementsInOrderAs Seq(Left("1 != 2 && 1 != 4"), Right(2), Left("3 != 2 && 3 != 4"), Right(4))
  }
}

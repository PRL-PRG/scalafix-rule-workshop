import org.scalatest.{FunSuite, Matchers => STMatchers}
import cats.syntax.semigroup._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{CallSite, Declaration, Location, Parent}
import cz.cvut.fit.prl.scalaimplicit.queries.Matchers
import org.scalatest.matchers.{MatchResult => STMatchResult, Matcher => STMatcher}

class DslTest extends FunSuite with STMatchers with Matchers {

  import cz.cvut.fit.prl.scalaimplicit.queries._
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
          // TODO: on of this is wrong - should be mismatched
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

  class X {
    def x = 1
  }

  class Y extends X {
    def y = 2
  }

  val x1 = new X
  val y1 = new Y

  val lx: List[X] = x1 :: y1 :: Nil
  val ly: List[Y] = y1 :: Nil


  test("is matcher matches an instance of ") {
    (2 matches is(2)) should matched
    (1 matches is(2)) should mismatched
  }

  test("combine matchers using default combinator AND") {
    val c = is(2) |+| is(1)
    c.description shouldBe "is 2 && is 1"
    c.negativeDescription shouldBe "is not 2 || is not 1"

    (2 matches c) should mismatched("2 is not 1")
    (1 matches c) should mismatched("1 is not 2")
  }

  test("negate matcher") {
    (1 matches !is(2)) should matched("1 is not 2")
  }

  test("negate negated matcher") {
    //noinspection DoubleNegationScala
    (1 matches !(!is(1))) should matched("1 is 1")
  }

  test("combine matchers using OR") {
    val c = is(1) || is(2) || is(3)
    c.description shouldBe "is 1 || is 2 || is 3"
    c.negativeDescription shouldBe "is not 1 && is not 2 && is not 3"

    (1 matches c) should matched("1 is 1")
    (2 matches c) should matched("2 is 2")
    (3 matches c) should matched("3 is 3")
    (4 matches c) should mismatched("4 is not 1 && 4 is not 2 && 4 is not 3")
  }

  test("plymorphism") {
    val mx = FunMatcher[X](v => v.x == 1, "", "")
    val my = FunMatcher[Y](v => v.y == 2, "", "")

    (x1 matches mx) should matched
    (y1 matches mx) should matched
    (y1 matches my) should matched
    assertDoesNotCompile("x1 matches my")

    val mlx = FunMatcher[List[X]](v => v.head.x == 1, "", "")
    val mly = FunMatcher[List[Y]](v => v.head.y == 2, "", "")

    (lx matches mlx) should matched
    (ly matches mlx) should matched
    (ly matches mly) should matched
    assertDoesNotCompile("lx matches mly")
  }

  test("Option") {
    //  (None matches allOf(2)) should mismatched("is empty")

    //    def matches[A](what: A, mather: Matcher[A]) = mather.matches(what)
    //
    //    matches[Container[Int]](Option(1), isEmpty())
    //
    //    matches(Option(1), isEmpty())
    //
    //    isEmpty[Int]().apply(Option(1))
    //
    //    Option(1) matches(isEmpty())

    //isEmpty().apply(Option(null))
    //(Option(null) matches isEmpty()) should matched("`None' is empty")
  }

  test("isEmpty - option") {
    (Option(1) matches isEmpty()) should mismatched("Some(1) is not empty")
    (Option(null) matches isEmpty()) should matched("None is empty")
    (None matches isEmpty()) should matched("None is empty")
  }

  test("isEmpty - sequences") {
    (Seq(1) matches isEmpty()) should mismatched("List(1) is not empty")
    (Nil matches isEmpty()) should matched("List() is empty")
    (List() matches isEmpty()) should matched("List() is empty")
  }

  test("isEmpty - map") {
    (Map(1 -> 2, 2 -> 3) matches isEmpty()) should mismatched("Map(1 -> 2, 2 -> 3) is not empty")
  }

  test("allOf - sequence") {
    (lx matches allOf(x1)) should matched
    (lx matches allOf(y1)) should matched

    val mx1: Matcher[List[X]] = allOf(y1)
    (lx matches mx1) should matched

    (Seq(1) matches allOf(1)) should matched("List(1) contains all of [1]")
    (Seq(1, 2) matches allOf(1, 2, 3, 4)) should mismatched("List(1, 2) is missing [3, 4]")
  }

  test("allOf -  option") {
    (Option(1) matches allOf(1)) should matched("Some(1) contains all of [1]")
    (None matches allOf(1)) should mismatched("None is missing [1]")
  }

  test("allOf - map") {
    (Map(1 -> 2) matches allOf(1 -> 2)) should matched("Map(1 -> 2) contains all of [(1,2)]")
    (Map(1 -> 2) matches allOf(1 -> 3)) should mismatched("Map(1 -> 2) is missing [(1,3)]")
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
  test("property matcher - size") {
    val x = 1 :: 2 :: Nil
    val m: Matcher[List[Int]] = Matchers.size(is(2))

    (x matches Matchers.size(is(2))) should matched("size 2 is 2")
    (Set(1,2) matches size(is(2))) should matched("size 2 is 2")
  }


  test("collection matching") {
    val q = is(2) || is(4)
    val col = Seq(1, 2, 3, 4)

    (col query q) should contain theSameElementsInOrderAs Seq(Mismatch(1, q), Match(2, q), Mismatch(3, q), Match(4, q))
    (col query q) map (_.toOption) should contain theSameElementsInOrderAs Seq(None, Some(2), None, Some(4))
    (col query q) map (_.toEither) should contain theSameElementsInOrderAs Seq(Left("1 is not 2 && 1 is not 4"), Right(2), Left("3 is not 2 && 3 is not 4"), Right(4))
  }

  test("callsite test") {

    import RepresentationMatchers._

    val cs = CallSite(
      "name-1",
      "code",
      Some(Location("file", 1, 2)),
      true,
      Declaration("decl-name", "def", Some(Location("fil",3,4)), true, None, Seq(Parent("parent", null, null))),
      null,
      null
    )

    cs.matches(
      name(startsWith("name")) &&
      declaration(
        kind(in("def", "val")) && location(contains(file(startsWith("file"))))
      )
    ) should mismatched("declaration location Some(fil:3:4) does not contain file that starts with \"file\"")

  }
}

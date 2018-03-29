package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.schema._
import org.scalatest.{FunSuite, Matchers => ScalaTestMatchers}

class MatchersTest extends FunSuite with ScalaTestMatchers with ScalaTestMatchingSupport with Matchers {


  import TestClasses._

  test("is matcher matches an instance of ") {
    (2 matches is(2)) should matched
    (1 matches is(2)) should mismatched
  }

  test("combine matchers using default combinator AND") {
    val c = and(is(2), is(1))
    c.description shouldBe "(is 2) && (is 1)"
    c.negativeDescription shouldBe "(is not 2) || (is not 1)"

    (2 matches c) should mismatched("(2) is not 1")
    (1 matches c) should mismatched("(1) is not 2")
  }

  test("negate matcher") {
    (1 matches !is(2)) should matched("(1) is not 2")
  }

  test("negate negated matcher") {
    //noinspection DoubleNegationScala
    (1 matches !(!is(1))) should matched("is 1")
  }

  test("combine matchers using OR") {
    val c = or(or(is(1), is(2)), is(3))
    c.description shouldBe "((is 1) || (is 2)) || (is 3)"
    c.negativeDescription shouldBe "((is not 1) && (is not 2)) && (is not 3)"

    (1 matches c) should matched("is 1")
    (2 matches c) should matched("is 2")
    (3 matches c) should matched("is 3")
    (4 matches c) should mismatched("(((4) is not 1) && ((4) is not 2)) && ((4) is not 3)")
  }

  test("polymorphism") {
    val mx = FunMatcher[X](v => v.x == 1, "", "")
    val my = FunMatcher[Y](v => v.y == 1, "", "")

    (x1 matches mx) should matched
    (y1 matches mx) should matched
    (y1 matches my) should matched
    assertDoesNotCompile("x1 matches my")

    val mlx = FunMatcher[List[X]](v => v.head.x == 1, "", "")
    val mly = FunMatcher[List[Y]](v => v.head.y == 1, "", "")

    (lx matches mlx) should matched
    (ly matches mlx) should matched
    (ly matches mly) should matched
    assertDoesNotCompile("lx matches mly")
  }

  //  test("case") {
  //    case class C(xx: Int, yy: Seq[X])
  //    case class D(xx: Int, yy: Seq[Y])
  //
  //    trait Pxx
  //    trait Pyy
  //
  //    implicit val Cxx: PG[C, Pxx, Int] = PG(_.xx)
  //    implicit val Dxx: PG[D, Pxx, Int] = PG(_.xx)
  //
  //    def xx[A](m: Matcher[Int])(implicit pg: PG[A, Pxx, Int]): Matcher[A] =
  //      PropertyMatcher("xx", m, Seq())
  //
  //    def yy(m: Matcher[Seq[X]]): Matcher[C] =
  //      PropertyMatcher("yy", _.yy, m, Seq())
  //
  //    val c = C(1, Seq(x1, y1))
  //    val d = D(2, Seq(y1))
  //
  //    val mc: Matcher[C] = and(xx(is(2)), yy(and(contains(is(y1)), size(1))))
  //    val md: Matcher[D] = xx(is(1))
  //
  //    (c matches and(mc, yy(allOf(y1)))) should matched("")
  //  }

  //  test("allOf -  option") {
  //    (Option(1) matches allOf(1)) should matched("contains all of [1]")
  //    val o: Option[Int] = None
  //    (o matches allOf(1)) should mismatched("None is missing [1]")
  //  }

  test("in") {
    (2 matches in(1, 2, 3)) should matched("is 2")
    (4 matches in(1, 2, 3)) should mismatched("(4) is not in [1, 2, 3]")
  }

  test("in regex") {
    ("final class" matches or(regex("class"), regex("def"))) should matched("matches Regex(\"class\")")
    ("val" matches or(regex("class"), regex("def"))) should mismatched("((\"val\") does not match Regex(\"class\")) && ((\"val\") does not match Regex(\"def\"))")
  }

  test("match result") {
    val q = or(is(2), is(4))
    val col = Seq(1, 2, 3, 4)

    (col query q) should contain theSameElementsInOrderAs Seq(Mismatch(1, q), Match(2, q), Mismatch(3, q), Match(4, q))
    (col query q) map (_.toOption) should contain theSameElementsInOrderAs Seq(None, Some(2), None, Some(4))
    (col query q) map (_.toEither) should contain theSameElementsInOrderAs Seq(
      Left("((1) is not 2) && ((1) is not 4)"),
      Right(2),
      Left("((3) is not 2) && ((3) is not 4)"),
      Right(4)
    )
  }

  // TODO: move to its own file
  test("callsite test") {

    import SchemaMatchers._

    val cs = CallSite(
      "name-1",
      "code",
      Some(Location("file", 1, 2)),
      true,
      Declaration("decl-name", "def", Some(Location("fil", 3, 4)), true, None, Seq(Parent("parent", Declaration("a.parent", "class", None, false, None, Seq()), Seq()))),
      Seq(),
      Seq()
    )

    val m = cs.matches(
      name(startsWith("name")),
      declaration(
        kind(in("def", "val")), location(file(startsWith("file")))
      )
    )

    m should mismatched("declaration location value file (\"fil\") does not start with \"file\"")

    println(m.matcher.description)
    println(m.matcher.negativeDescription)
  }
}

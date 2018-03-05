package cz.cvut.fit.prl.scalaimplicit.matcher

import org.scalatest.{FunSuite, Matchers => ScalaTestMatchers}
import cats.syntax.semigroup._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._
import cz.cvut.fit.prl.scalaimplicit.queries.PG
import cz.cvut.fit.prl.scalaimplicit.matcher.implicits._

class MatchersTest extends FunSuite with ScalaTestMatchers with ScalaTestMatchingSupport with Matchers {

  object TestClasses {

    class W

    class X(val x: Int) extends W {
      override def toString: String = s"X($x)"
    }

    class Y(x: Int, val y: Int) extends X(x) {
      override def toString: String = s"Y($x,$y)"
    }

    val w1 = new W
    val x1 = new X(1)
    val y1 = new Y(1, 1)

    val lw: List[W] = w1 :: x1 :: y1 :: Nil
    val lx: List[X] = x1 :: y1 :: Nil
    val ly: List[Y] = y1 :: Nil
  }

  import TestClasses._

  test("is matcher matches an instance of ") {
    (2 matches is(2)) should matched
    (1 matches is(2)) should mismatched
  }

  test("combine matchers using default combinator AND") {
    val c = and(is(2), is(1))
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
    (1 matches !(!is(1))) should matched("is 1")
  }

  test("combine matchers using OR") {
    val c = or(or(is(1), is(2)), is(3))
    c.description shouldBe "is 1 || is 2 || is 3"
    c.negativeDescription shouldBe "is not 1 && is not 2 && is not 3"

    (1 matches c) should matched("is 1")
    (2 matches c) should matched("is 2")
    (3 matches c) should matched("is 3")
    (4 matches c) should mismatched("4 is not 1 && 4 is not 2 && 4 is not 3")
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

  //  test("isEmpty - option") {
  //    (Option(1) matches isEmpty) should mismatched("Some(1) is not empty")
  //    (Option(null) matches isEmpty) should matched("is empty")
  //    (None matches isEmpty) should matched("is empty")
  //  }

  test("isEmpty - sequences") {
    (Seq(1) matches isEmpty) should mismatched("List(1) is not empty")
    (Nil matches isEmpty) should matched("is empty")
    (List() matches isEmpty) should matched("is empty")
  }

  test("isEmpty - map") {
    (Map(1 -> 2, 2 -> 3) matches isEmpty) should mismatched("Map(1 -> 2, 2 -> 3) is not empty")
  }

  test("allOf - sequence") {
    (lx matches allOf(x1)) should matched
    //    (lx matches allOf(y1)) should matched

    lx.contains(w1)
    lx.contains(x1)
    lx.contains(y1)

    val xx: Matcher[List[X]] = allOf(y1)
    val yy: Matcher[List[Y]] = allOf(y1)

    val yyy: List[X] = ly

    //    val mx1: Matcher[List[X]] = allOf(y1)
    //    (lx matches mx1) should matched

    (Seq(1) matches allOf(1)) should matched("contains all of [1]")
    (Seq(1, 2) matches allOf(1, 2, 3, 4)) should mismatched("List(1, 2) is missing [3, 4]")
  }

  test("aaaa") {
    (lx matches and(contains(is(x1)), contains(is(y1)))) should matched("contains [is X(1)] && contains [is Y(1,1)]")
    (lx matches contains(is(x1), is(y1))) should matched("contains [is X(1), is Y(1,1)]")
    (lx matches and(allOf(x1), allOf(y1))) should matched("contains [is X(1)] && contains [is Y(1,1)]")
    (lx matches allOf(x1, y1)) should matched("contains [is X(1), is Y(1,1)]")

    (lx matches and(size(1), allOf(y1))) should matched("contains [is X(1)] && contains [is Y(1,1)]")

    (lx matches and(contains(is(x1)), contains(is(y1))))
    (lx matches and(contains(is(y1)), contains(is(x1))))

    (lx matches and(allOf(x1), size(is(2))))
    (lx matches allOf(y1))

    (lx matches and(allOf(y1), allOf(x1)))
    (lx matches and(allOf(x1), allOf(y1)))
    (lx matches allOf(y1, x1, 1))
    //    (x1 matches allOf(y1, x1, 1))

    lx matches is(List(lx))
  }

  test("case") {
    case class C(xx: Int, yy: Seq[X])
    case class D(xx: Int, yy: Seq[Y])

    trait Pxx
    trait Pyy

    implicit val Cxx: PG[C, Pxx, Int] = PG(_.xx)
    implicit val Dxx: PG[D, Pxx, Int] = PG(_.xx)

    def xx[A](m: Matcher[Int])(implicit pg: PG[A, Pxx, Int]): Matcher[A] =
      PropertyMatcher("xx", m, Seq())

    def yy(m: Matcher[Seq[X]]): Matcher[C] =
      PropertyMatcher("yy", _.yy, m, Seq())

    val c = C(1, Seq(x1, y1))
    val d = D(2, Seq(y1))

    val mc: Matcher[C] = and(xx(is(2)), yy(and(contains(is(y1)), size(1))))
    val md: Matcher[D] = xx(is(1))

    (c matches and(mc, yy(allOf(y1)))) should matched("")
  }

  //  test("allOf -  option") {
  //    (Option(1) matches allOf(1)) should matched("contains all of [1]")
  //    val o: Option[Int] = None
  //    (o matches allOf(1)) should mismatched("None is missing [1]")
  //  }

  test("allOf - map") {
    (Map(1 -> 2) matches allOf(1 -> 2)) should matched("contains all of [(1,2)]")
    (Map(1 -> 2) matches allOf(1 -> 3)) should mismatched("Map(1 -> 2) is missing [(1,3)]")
  }

  test("in") {
    (in(1, 2, 3) matches 2) should matched("is 2")
    (in(1, 2, 3) matches 4) should mismatched("4 is not in [1, 2, 3]")
  }

  test("in regex") {
    (in("class".r, "def".r) matches "final class") should matched("matches Regex(\"class\")")
    (in("class".r, "def".r) matches "val") should mismatched("\"val\" is not in [Regex(\"class\"), Regex(\"def\")]")
  }

  test("cd") {
    Seq(1) matches allOf(1)
    //    (Seq(1) matches allOf(1) && allOf(2) && contains(is(3)) && allOf(1)) should matched("")
    //    (Seq(1, 2) matches size(is(1)) && isEmpty && contains(is(1)) && size(is(1))) should matched("")
  }

  test("comb") {
    //    //Seq(1) matches (size(is(1)) && contains(is(1)))
    //    val x : Matcher[Iterable[Int]] = contains(is(1)) && contains(is(1))
    //    Seq(1) matches (x)
    //    Seq(1) matches (contains(is(1)) ++ contains(is(1)))
  }

  //
  //  test("ordering") {
  //    gt(2) matches 3
  //  }
  //

  test("size") {
    (Set(1, 2) matches size(is(2))) should matched("size is 2")
    (Set(1) matches size(is(2))) should mismatched("Set(1) size is not 2")
  }


  test("collection matching") {
    val q = or(is(2), is(4))
    val col = Seq(1, 2, 3, 4)

    (col query q) should contain theSameElementsInOrderAs Seq(Mismatch(1, q), Match(2, q), Mismatch(3, q), Match(4, q))
    (col query q) map (_.toOption) should contain theSameElementsInOrderAs Seq(None, Some(2), None, Some(4))
    (col query q) map (_.toEither) should contain theSameElementsInOrderAs Seq(Left("1 is not 2 && 1 is not 4"), Right(2), Left("3 is not 2 && 3 is not 4"), Right(4))
  }

  //  test("pgs") {
  //
  //    case class C(b1: Boolean, b2: Boolean, b3: Boolean)
  //
  //    //    def b1[A <: {def b1: Boolean}]: Matcher[A] =
  //    //      PropertyMatcher[A]("b1", _.b1)
  //    //    def b2[A <: {def b2: Boolean}]: Matcher[A] =
  //    //      PropertyMatcher[A]("b1", _.b2)
  //    //    def b3[A <: {def b3: Boolean}]: Matcher[A] =
  //    //      PropertyMatcher[A]("b1", _.b3)
  //
  //    trait X {
  //      type B1
  //      type B2
  //      type B3
  //
  //      implicit val PG_C_b1 = PG[C, B1, Boolean](_.b1)
  //      implicit val PG_C_b2 = PG[C, B2, Boolean](_.b2)
  //      implicit val PG_C_b3 = PG[C, B3, Boolean](_.b3)
  //
  //      def b1[A](implicit pg: PG[A, B1, Boolean]): Matcher[A] = BooleanPropertyMatcher("b1")
  //
  //      def b2[A](implicit pg: PG[A, B2, Boolean]): Matcher[A] = BooleanPropertyMatcher("b2")
  //
  //      def b3[A](implicit pg: PG[A, B3, Boolean]): Matcher[A] = BooleanPropertyMatcher("b3")
  //
  //      type Size
  //
  //      implicit val PG_OPTION_SIZE = PG[Option[_], Size, Int](x => if (x.isEmpty) 0 else 1)
  //      implicit val PG_Iterable_Size = PG[Iterable[_], Size, Int](_.size)
  //
  //      type Contains
  //
  //      def size1[A](x: Matcher[Int])(implicit pg: PG[A, Size, Int]): Matcher[A] = PropertyMatcher("size", x, Nil)
  //    }
  //
  //    object X extends X
  //    import X._
  //
  //    println(
  //      (Seq(C(true, true, true), C(true, true, true)) matches contains(b1 && !b2 && b3)).reason
  //    )
  //
  //    println(C(true, false, true) matches b1 && !b2 && b3)
  //
  //    println(Option(1) matches size1(is(1)))
  //    println(Seq(1, 2) matches size1(is(2)))
  //
  //  }
  //
  //
  //  test("pgs2") {
  //    case class C(b1: Boolean, b2: Boolean, b3: Boolean)
  //
  //    //    trait PG2[In] {
  //    //      type Out
  //    //
  //    //      def f(x: In): Out
  //    //    }
  //    //
  //    //    object PG2 {
  //    //      type Aux[A, B] = PG2[A] { type Out = B }
  //    //
  //    //      def apply[A, B](ff: A => B): Aux[A, B] = new PG2[A] {
  //    //        override type Out = B
  //    //
  //    //        override def f(x: A): B = ff(x)
  //    //      }
  //    //    }
  //    //
  //    //    implicit val cb1 = PG2[C, Boolean](_.b1)
  //    //
  //    //    def b1[A](implicit pg2: PG2.Aux[A, Boolean]): Matcher[A] =
  //    //      new FunMatcher(pg2.f, "", "")
  //
  //    trait X {
  //      type B1
  //      type B2
  //      type B3
  //
  //      implicit val PG_C_b1 = PG[C, B1, Boolean](_.b1)
  //      implicit val PG_C_b2 = PG[C, B2, Boolean](_.b2)
  //      implicit val PG_C_b3 = PG[C, B3, Boolean](_.b3)
  //
  //      def b1[A](implicit pg: PG[A, B1, Boolean]): Matcher[A] = BooleanPropertyMatcher("b1")
  //
  //      def b2[A](implicit pg: PG[A, B2, Boolean]): Matcher[A] = BooleanPropertyMatcher("b2")
  //
  //      def b3[A](implicit pg: PG[A, B3, Boolean]): Matcher[A] = BooleanPropertyMatcher("b3")
  //
  //      type Size
  //
  //      implicit val PG_OPTION_SIZE = PG[Option[_], Size, Int](x => if (x.isEmpty) 0 else 1)
  //      implicit val PG_Iterable_Size = PG[Iterable[_], Size, Int](_.size)
  //
  //      type Contains
  //
  //      def size1[A](x: Matcher[Int])(implicit pg: PG[A, Size, Int]): Matcher[A] = PropertyMatcher("size", x, Nil)
  //    }
  //
  //    object X extends X
  //    import X._
  //
  //    println(
  //      (Seq(C(true, true, true), C(true, true, true)) matches contains(b1 && !b2 && b3)).reason
  //    )
  //
  //    println(C(true, false, true) matches b1 && !b2 && b3)
  //
  //    println(Option(1) matches size1(is(1)))
  //    println(Seq(1, 2) matches size1(is(2)))
  //
  //  }

  test("Imports") {
    import SchemaMatchers._

    CallSite("A", null, None, true, null, Seq(), Seq()) matches name(is("A"))
    DeclaredParameter("B", null) matches name(is("A"))
  }

  //  test("callsite test") {
  //
  //    import cz.cvut.fit.prl.scalaimplicit.queries.SchemaMatchers._
  //
  //    val cs = CallSite(
  //      "name-1",
  //      "code",
  //      Some(Location("file", 1, 2)),
  //      true,
  //      Declaration("decl-name", "def", Some(Location("fil",3,4)), true, None, Seq(Parent("parent", null, null))),
  //      null,
  //      null
  //    )
  //
  //    val m = cs.matches(
  //      name(startsWith("name")) &&
  //      declaration(
  //        kind(in("def", "val")) && location(contains(file(startsWith("file"))))
  //      )
  //    )
  //
  //    m should mismatched("declaration location Some(fil:3:4) does not contain file that starts with \"file\"")
  //
  //    println(m.matcher.description)
  //    println(m.matcher.negativeDescription)
  //  }
}

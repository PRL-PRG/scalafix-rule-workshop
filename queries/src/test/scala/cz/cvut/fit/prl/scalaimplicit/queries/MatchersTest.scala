package cz.cvut.fit.prl.scalaimplicit.queries

import org.scalatest.{FunSuite, Matchers => ScalaTestMatchers}
import cats.syntax.semigroup._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._
import cz.cvut.fit.prl.scalaimplicit.queries.implicits._

class MatchersTest extends FunSuite with ScalaTestMatchers with ScalaTestMatchingSupport with Matchers {

  object TestClasses {

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
  }

  import TestClasses._

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

  test("polymorphism") {
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

  test("isEmpty - option") {
    (Option(1) matches isEmpty) should mismatched("Some(1) is not empty")
    (Option(null) matches isEmpty) should matched("None is empty")
    (None matches isEmpty) should matched("None is empty")
  }

  test("isEmpty - sequences") {
    (Seq(1) matches isEmpty) should mismatched("List(1) is not empty")
    (Nil matches isEmpty) should matched("List() is empty")
    (List() matches isEmpty) should matched("List() is empty")
  }

  test("isEmpty - map") {
    (Map(1 -> 2, 2 -> 3) matches isEmpty) should mismatched("Map(1 -> 2, 2 -> 3) is not empty")
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

  test("in") {
    (in(1, 2, 3) matches 2) should matched("2 is 2")
    (in(1, 2, 3) matches 4) should mismatched("4 is not in [1, 2, 3]")
  }

  test("in regex") {
    (in("class".r, "def".r) matches "final class") should matched("\"final class\" matches Regex(\"class\")")
    (in("class".r, "def".r) matches "val") should mismatched("\"val\" is not in [Regex(\"class\"), Regex(\"def\")]")
  }
  //
  //  test("ordering") {
  //    gt(2) matches 3
  //  }
  //

  test("property matcher - size") {
    val x = 1 :: 2 :: Nil
    val m: Matcher[List[Int]] = Matchers.size(is(2))

    (x matches Matchers.size(is(2))) should matched("size 2 is 2")
    (Set(1, 2) matches size(is(2))) should matched("size 2 is 2")
  }


  test("collection matching") {
    val q = is(2) || is(4)
    val col = Seq(1, 2, 3, 4)

    (col query q) should contain theSameElementsInOrderAs Seq(Mismatch(1, q), Match(2, q), Mismatch(3, q), Match(4, q))
    (col query q) map (_.toOption) should contain theSameElementsInOrderAs Seq(None, Some(2), None, Some(4))
    (col query q) map (_.toEither) should contain theSameElementsInOrderAs Seq(Left("1 is not 2 && 1 is not 4"), Right(2), Left("3 is not 2 && 3 is not 4"), Right(4))
  }

  test("pgs") {

    case class C(b1: Boolean, b2: Boolean, b3: Boolean)

    //    def b1[A <: {def b1: Boolean}]: Matcher[A] =
    //      PropertyMatcher[A]("b1", _.b1)
    //    def b2[A <: {def b2: Boolean}]: Matcher[A] =
    //      PropertyMatcher[A]("b1", _.b2)
    //    def b3[A <: {def b3: Boolean}]: Matcher[A] =
    //      PropertyMatcher[A]("b1", _.b3)

    trait X {
      type B1
      type B2
      type B3

      implicit val PG_C_b1 = PG[C, B1, Boolean](_.b1)
      implicit val PG_C_b2 = PG[C, B2, Boolean](_.b2)
      implicit val PG_C_b3 = PG[C, B3, Boolean](_.b3)

      def b1[A](implicit pg: PG[A, B1, Boolean]): Matcher[A] = BooleanPropertyMatcher("b1")

      def b2[A](implicit pg: PG[A, B2, Boolean]): Matcher[A] = BooleanPropertyMatcher("b2")

      def b3[A](implicit pg: PG[A, B3, Boolean]): Matcher[A] = BooleanPropertyMatcher("b3")

      type Size

      implicit val PG_OPTION_SIZE = PG[Option[_], Size, Int](x => if (x.isEmpty) 0 else 1)
      implicit val PG_Iterable_Size = PG[Iterable[_], Size, Int](_.size)

      type Contains

      def size1[A](x: Matcher[Int])(implicit pg: PG[A, Size, Int]): Matcher[A] = PropertyMatcher("size", x, Nil)
    }

    object X extends X
    import X._

    println(
      (Seq(C(true, true, true), C(true, true, true)) matches contains(b1 && !b2 && b3)).reason
    )

    println(C(true, false, true) matches b1 && !b2 && b3)

    println(Option(1) matches size1(is(1)))
    println(Seq(1, 2) matches size1(is(2)))

  }


  test("pgs2") {
    case class C(b1: Boolean, b2: Boolean, b3: Boolean)

    //    trait PG2[In] {
    //      type Out
    //
    //      def f(x: In): Out
    //    }
    //
    //    object PG2 {
    //      type Aux[A, B] = PG2[A] { type Out = B }
    //
    //      def apply[A, B](ff: A => B): Aux[A, B] = new PG2[A] {
    //        override type Out = B
    //
    //        override def f(x: A): B = ff(x)
    //      }
    //    }
    //
    //    implicit val cb1 = PG2[C, Boolean](_.b1)
    //
    //    def b1[A](implicit pg2: PG2.Aux[A, Boolean]): Matcher[A] =
    //      new FunMatcher(pg2.f, "", "")

    trait X {
      type B1
      type B2
      type B3

      implicit val PG_C_b1 = PG[C, B1, Boolean](_.b1)
      implicit val PG_C_b2 = PG[C, B2, Boolean](_.b2)
      implicit val PG_C_b3 = PG[C, B3, Boolean](_.b3)

      def b1[A](implicit pg: PG[A, B1, Boolean]): Matcher[A] = BooleanPropertyMatcher("b1")

      def b2[A](implicit pg: PG[A, B2, Boolean]): Matcher[A] = BooleanPropertyMatcher("b2")

      def b3[A](implicit pg: PG[A, B3, Boolean]): Matcher[A] = BooleanPropertyMatcher("b3")

      type Size

      implicit val PG_OPTION_SIZE = PG[Option[_], Size, Int](x => if (x.isEmpty) 0 else 1)
      implicit val PG_Iterable_Size = PG[Iterable[_], Size, Int](_.size)

      type Contains

      def size1[A](x: Matcher[Int])(implicit pg: PG[A, Size, Int]): Matcher[A] = PropertyMatcher("size", x, Nil)
    }

    object X extends X
    import X._

    println(
      (Seq(C(true, true, true), C(true, true, true)) matches contains(b1 && !b2 && b3)).reason
    )

    println(C(true, false, true) matches b1 && !b2 && b3)

    println(Option(1) matches size1(is(1)))
    println(Seq(1, 2) matches size1(is(2)))

  }

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

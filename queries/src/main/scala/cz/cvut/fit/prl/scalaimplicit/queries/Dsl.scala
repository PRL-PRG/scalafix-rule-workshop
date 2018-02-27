package cz.cvut.fit.prl.scalaimplicit.queries

import cats._
import cats.implicits._
import com.sun.org.glassfish.gmbal.Description
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{CallSite, Declaration, Location, Type}

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.reflectiveCalls
import scala.language.implicitConversions


sealed trait MatchResult[A] {
  def instance: A

  def matches: Boolean

  def matcher: Matcher[A]

  def reason: String

  def toOption: Option[A] = if (matches) Some(instance) else None

  def toEither: Either[String, A] = if (matches) Right(instance) else Left(reason)
}

case class Match[A](instance: A, matcher: Matcher[A]) extends MatchResult[A] {
  override def matches: Boolean = true

  override def reason: String = matcher.describeMatch(instance).get
}

case class Mismatch[A](instance: A, matcher: Matcher[A]) extends MatchResult[A] {
  override def matches: Boolean = false

  override def reason: String = matcher.describeMismatch(instance).get
}

trait Matcher[-A] {
  self =>

  def test(v: A): Boolean

  def description: String

  def negativeDescription: String

  def describeMatch(v: A): Option[String]

  def describeMismatch(v: A): Option[String]

  def matches[A1 <: A](v: A1): MatchResult[A1] = if (test(v)) {
    Match(v, this)
  } else {
    Mismatch(v, this)
  }

  def &&[A1 <: A](other: Matcher[A1]): Matcher[A1] = new Matcher[A1] {
    override def test(v: A1): Boolean = self.test(v) && other.test(v)

    override def description: String = self.description + " && " + other.description

    override def negativeDescription: String = self.negativeDescription + " || " + other.negativeDescription

    override def describeMatch(v: A1): Option[String] = self.matches(v) match {
      case result if result.matches => self.describeMatch(v)
      case _ => None
    }

    override def describeMismatch(v: A1): Option[String] = (self.describeMismatch(v), other.describeMismatch(v)) match {
      case (Some(m1), Some(m2)) => Some(m1 + " || " + m2)
      case (m1@Some(_), None) => m1
      case (None, m2@Some(_)) => m2
      case _ => None
    }
  }

  def ||[A1 <: A](other: Matcher[A1]): Matcher[A1] = new Matcher[A1] {
    override def test(v: A1): Boolean = self.test(v) || other.test(v)

    override def description: String = self.description + " || " + other.description

    override def negativeDescription: String = self.negativeDescription + " && " + other.negativeDescription

    override def describeMatch(v: A1): Option[String] = self.matches(v) match {
      case result if result.matches => self.describeMatch(v)
      case _ => other.describeMatch(v)
    }

    override def describeMismatch(v: A1): Option[String] =
      self.describeMismatch(v).map(_ + " && ") |+| other.describeMismatch(v)
  }

  def unary_!(): Matcher[A] = new Matcher[A] {
    override def test(v: A): Boolean = !self.test(v)

    override def description: String = self.negativeDescription

    override def negativeDescription: String = self.description

    override def describeMatch(v: A): Option[String] = self.describeMismatch(v)

    override def describeMismatch(v: A): Option[String] = self.describeMatch(v)
  }
}

// TODO: clean up
trait Matchers {
  self =>

  // TODO: make it a default instance of some Pretifier
  def fmt[A](s: A): String = s match {
    case x: String => "\"" + x + "\""
    case x: Char => s"'$x'"
    case x@(_: Boolean | _: Byte | _: Short | _: Int | _: Long | _: Float | _: Double) => s"$x"
//    case x: Map[_, _] => x.map { case (k, v) => fmt(k) + ": " + fmt(v) }.mkString("{", ", ", "}")
    case x => x.toString
  }

  def fmtRight[A](s : A): String = s match {
    case x: Traversable[_] => x.map(fmt).mkString("[", ", ", "]")
  }

  trait ImplicitMatchers {

    // implicit def any2matcher[A](x: A): Matcher[A] = is(x)

    implicit class QueryCollectionSupport[A, +Repr](that: TraversableLike[A, Repr]) {
      def query[That](matcher: Matcher[A])(implicit bf: CanBuildFrom[Repr, MatchResult[A], That]): That =
        that.map(matcher.matches)
    }

    implicit class AnyMatcher[A](that: A) {
      def matches(matcher: Matcher[A]): MatchResult[A] = matcher.matches(that)
    }

  }

  object implicits extends ImplicitMatchers

  // default Matcher combinator is AND
  implicit def matcherSemigroup[A]: Semigroup[Matcher[A]] = (x: Matcher[A], y: Matcher[A]) => x && y

  // TODO: isn't this in cats already?
  private def reduce[A: Semigroup](x: A, xs: Seq[A]): A = xs match {
    case Seq() => x
    case Seq(y, ys@_*) => reduce(x |+| y, ys)
  }

  abstract class AbstractMatcher[-A](val description: String, val negativeDescription: String) extends Matcher[A] {
    override def describeMatch(v: A): Option[String] = {
      if (test(v)) {
        Some(fmt(v) + " " + description)
      } else {
        None
      }
    }

    override def describeMismatch(v: A): Option[String] = {
      if (!test(v)) {
        Some(fmt(v) + " " + negativeDescription)
      } else {
        None
      }
    }
  }

  class FunMatcher[-A](val fun: A => Boolean, description: String, negativeDescription: String)
    extends AbstractMatcher[A](description, negativeDescription) {

    override def test(v: A): Boolean = fun(v)
  }

  object FunMatcher {
    def apply[A](fun: A => Boolean, description: String, negativeDescription: String): FunMatcher[A] =
      new FunMatcher[A](fun, description, negativeDescription)
  }

  class PropertyMatcher[A, B](val name: String, private val property: A => B, private val matcher: Matcher[B]) extends Matcher[A] {

    override def description: String = name + " that " + matcher.description

    override def negativeDescription: String = name + " that " + matcher.negativeDescription

    override def describeMatch(v: A): Option[String] = matcher.describeMatch(property(v)).map(name + " " + _)

    override def describeMismatch(v: A): Option[String] = matcher.describeMismatch(property(v)).map(name + " " + _)

    override def test(v: A): Boolean = matcher.test(property(v))
  }

  object PropertyMatcher {
    def apply[A, B](name: String, property: A => B, matcher: Matcher[B], matchers: Seq[Matcher[B]]) =
      new PropertyMatcher[A, B](name, property, reduce(matcher, matchers))
  }

  class PropertyMatchHelper[A, B](val name: String, val property: A => B) {
    def ==(x: B): Matcher[A] = new PropertyMatcher[A, B](name, property, is(x))

    def !=(x: B): Matcher[A] = new PropertyMatcher[A, B](name, property, not(is(x)))
  }

  trait OrderingMatchHelper[A, B] extends PropertyMatchHelper[A, B] {
    def >(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, gt(x))

    def <(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, lt(x))

    def >=(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, gteq(x))

    def <=(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, lteq(x))
  }

  trait StringMatchHelper[A] extends PropertyMatchHelper[A, String] {
    def startsWith(prefix: String): Matcher[A] =
      new PropertyMatcher[A, String](name, property, self.startsWith(prefix))
  }

  trait OptionMatchHelper[A, B] extends PropertyMatchHelper[A, Option[B]] {

  }

  def is[A](x: A): Matcher[A] =
    FunMatcher[A](v => v == x, s"is ${fmt(x)}", s"is not ${fmt(x)}")

  def not[A](x: Matcher[A]): Matcher[A] = !x

  def gt[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].gt(v, x), s"is > ${fmt(x)}", s"is < ${fmt(x)}")

  def lt[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].lt(v, x), s"is < ${fmt(x)}", s"is > ${fmt(x)}")

  def gteq[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].gteq(v, x), s"is >= ${fmt(x)}", s"is <= ${fmt(x)}")

  def lteq[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].lteq(v, x), s"is <= ${fmt(x)}", s"is >= ${fmt(x)}")

  // matchers for collections
  def contains[A, B](x: Matcher[A]): Matcher[B] = {
    new AbstractMatcher[B](s"contains ${x.description}", s"does not contain ${x.description}") {
      def exists(v: B, p: A => Boolean): Boolean = v match {
        case v: Option[A] => v.exists(p)
        case v: Seq[A] => v.exists(p)
        case v: Traversable[A] => v.exists(p)
        case _ => throw new IllegalArgumentException(s"${v.getClass} is not supported")
      }

      override def test(v: B): Boolean = exists(v, x.test)
    }
  }

  // TODO: the B should be constrained, but I did not figure out any reasonable way
  // that will work with different containers (e.g. Option, Traversable, ...)
  // Ideally there should be a type class that would provide a the needed
  // functionality
  def allOf[A, B](x: A, xs: A*): Matcher[B] = {
    val all = x +: xs

    new AbstractMatcher[B](s"contains all of ${fmtRight(all)}", s"does not contain all of ${fmtRight(all)}") {
      def contains(v: B, e: A): Boolean = v match {
        case v: Option[A] => v.contains(e)
        case v: Seq[A] => v.contains(e)
        case v: Traversable[A] => v.exists(_ == e)
        case _ => throw new IllegalArgumentException(s"${v.getClass} is not supported")
      }

      def missing(v: B): Seq[A] = all.filterNot(e => contains(v, e))

      override def test(v: B): Boolean = missing(v).isEmpty

      override def describeMismatch(v: B): Option[String] =
        if (!test(v)) Some(s"${fmt(v)} is missing ${fmtRight(missing(v))}") else None
    }
  }

  def isEmpty[A <: {def isEmpty : Boolean}](): Matcher[A] =
    FunMatcher(v => v.isEmpty, "is empty", "is not empty")

  // TODO: types(anyOf(1,2,3))
  // TODO: types(only(1,2,3))
  // TODO: types(inOrderOnly(1,2,3))
  // TODO: types(inOrder(1,2,3))

  // TODO x, xs
  // TODO: fmt
  def in[A](xs: A*): Matcher[A] =
    FunMatcher[A](
      v => xs.contains(v),
      "in " + xs.map(x => fmt(x)).mkString("{", ",", "}"),
      "not in " + xs.map(x => fmt(x)).mkString("{", ",", "}")
    )

  def startsWith(x: String): Matcher[String] =
    FunMatcher(
      v => v.startsWith(x),
      s"starts with ${fmt(x)}",
      s"does not start with ${fmt(x)}"
    )

  // TODO: create a macro for this
  def size[A, B <: {def size : A}](x: Matcher[A], xs: Matcher[A]*): Matcher[B] =
    PropertyMatcher[B, A]("size", v => v.size, x, xs)

  def size[A, B <: {def size : A}] = new PropertyMatchHelper[B, A]("size", v => v.size)
}

object Matchers extends Matchers

trait RepresentationMatchers {

  import Matchers._
  /*
    case class Declaration(name: String,
                           kind: String,
                           location: Option[Location],
                           isImplicit: Boolean,
                           signature: Option[Signature] = None,
                           parents: Seq[Parent] = Seq())


   case class CallSite(name: String,
                        code: String,
                        location: Option[Location],
                        isSynthetic: Boolean,
                        declaration: Declaration,
                        typeArguments: Seq[Type],
                        implicitArguments: Seq[ArgumentLike])
   */


  def name[A <: {def name : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher[A, String]("name", v => v.name, x, xs)

  def name[A <: {def name : String}] =
    new PropertyMatchHelper[A, String]("name", v => v.name) with StringMatchHelper[A]

  def kind[A <: {def kind : String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher[A, String]("kind", v => v.kind, x, xs)

  def kind[A <: {def kind : String}] =
    new PropertyMatchHelper[A, String]("kind", v => v.kind) with StringMatchHelper[A]

  def declaration[A <: {def declaration: Declaration}](x: Matcher[Declaration], xs: Matcher[Declaration]*): Matcher[A] =
    PropertyMatcher[A, Declaration]("declaration", v => v.declaration, x, xs)

  def file[A <: {def file: String}](x: Matcher[String], xs: Matcher[String]*): Matcher[A] =
    PropertyMatcher[A, String]("file", v => v.file, x, xs)
  def file[A <: {def file: String}] =
    new PropertyMatchHelper[A, String]("file", v => v.file) with StringMatchHelper[A]

  def location[A <: {def location: Option[Location]}](x: Matcher[Option[Location]], xs: Matcher[Option[Location]]*): Matcher[A] =
    PropertyMatcher[A, Option[Location]]("location", v => v.location, x, xs)
  def location[A <: {def location: Option[Location]}] =
    new PropertyMatchHelper[A, Option[Location]]("location", v => v.location) with OptionMatchHelper[A, Location]
}

object RepresentationMatchers extends RepresentationMatchers

//
//
//
//object Test {
//
//  import X._
//
//  val x = CallSite("c1", "", None, true, null, Seq(Type("A")), Seq())
//  val y = CallSite("c2", "", None, true, null, Seq(), Seq())
//  val cs = Seq(x, y)
//
//  // size(typeArguments) > 1
//  // typeArguments.size > 1
//
//  // x.matches(name(startsWith("A") && endsWith("B"))
//  // x.matches(name starsWith "A" && name endsWith "B")
//  // x.matches(startsWith(name, "A") && endsWith(name, "B"))
//
//  // x.matches(size(typeArguments) > 1)
//  x.matches(typeArguments.size > 1 && (typeArguments.size < 2 || typeArguments.size < 2))
//  x.matches(typeArguments.size > 1)
//  x.matches(typeArguments(size > 1))
//  x.matches(typeArguments(size(gt(1))))
//
//  cs.query(typeArguments(size(gt(1))))
//
//  /*
//  *
//  val xs: Seq[CallSite] = ...
//  xs.query(
//    synthetic == true,
//    declaration(
//      kind == "def",
//      signature(
//        parameterLists(
//          all(
//            `implicit` == true,
//            size == 1,
//            params(first(size == 1))
//          )
//        )
//      )
//    )
//  )
//  xs.query(isSynthetic, declaration(kind == "def", signature(parameterLists(all(isImplicit, size == 1, params(first(size == 1)))))))
//
//  xs.filter(x => x.isSynthetic && x.declaration.kind == "def", x.signature.parameterLists.forall(y => y.isImplicit && y.size == 1 && y.params.firstHead.map(_.size == 1).getOrElse(false))
//  * */
//}

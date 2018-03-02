package cz.cvut.fit.prl.scalaimplicit.queries

import cats._
import cats.implicits._

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.util.matching.Regex

trait ImplicitMatchers {

  // TODO: numbers, strings, bools
  // implicit def any2matcher[A](x: A): Matcher[A] = is(x)

  implicit class QueryCollectionSupport[A, +Repr](that: TraversableLike[A, Repr]) {
    def query[That](matcher: Matcher[A])(implicit bf: CanBuildFrom[Repr, MatchResult[A], That]): That =
      that.map(matcher.matches)
  }

  implicit class AnyMatcher[A](that: A) {
    def matches(matcher: Matcher[A]): MatchResult[A] = matcher.matches(that)
  }

}

// TODO: clean up
trait Matchers {
  self =>

  // TODO: make it a default instance of some Pretifier
  def fmt[A](s: A): String = s match {
    case x: Regex => "Regex(\"" + x + "\")"
    case x: String => "\"" + x + "\""
    case x: Char => s"'$x'"
    case x@(_: Boolean | _: Byte | _: Short | _: Int | _: Long | _: Float | _: Double) => s"$x"
    //    case x: Map[_, _] => x.map { case (k, v) => fmt(k) + ": " + fmt(v) }.mkString("{", ", ", "}")
    case x => x.toString
  }

  def fmtRight[A](s: A): String = s match {
    case x: Traversable[_] => x.map(fmt).mkString("[", ", ", "]")
  }

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

  /**
    * Property getter - a helper class for type-safe properties
    *
    * @param get
    * @tparam A
    * @tparam B a marker type
    * @tparam C
    */
  case class PG[-A, B, +C](get: A => C)

  object PropertyMatcher {
    def apply[A, B](name: String, property: A => B, matcher: Matcher[B], matchers: Seq[Matcher[B]]): Matcher[A] =
      new PropertyMatcher[A, B](name, property, reduce(matcher, matchers))

    def apply[A, B](name: String, matcher: Matcher[B], matchers: Seq[Matcher[B]] = Seq())
                   (implicit pg: PG[A, _, B]): Matcher[A] =
      apply(name, pg.get, matcher, matchers)
  }

  object BooleanPropertyMatcher {
    def apply[A](name: String, property: A => Boolean): Matcher[A] =
      new PropertyMatcher[A, Boolean](name, property, is(true)) {
        override def description: String = s"is $name"

        override def negativeDescription: String = s"is not $name"

        override def describeMatch(v: A): Option[String] =
          if (test(v)) Some(s"$v is $name") else None

        override def describeMismatch(v: A): Option[String] =
          if (!test(v)) Some(s"$v is not $name") else None
      }

    def apply[A](name: String)(implicit pg: PG[A, _, Boolean]): Matcher[A] = apply(name, pg.get)
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

  trait SeqMatchHelper[A, B] extends PropertyMatchHelper[A, Seq[B]] {

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
      // TODO: B should be constrained
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

  def isEmpty[A <: {def isEmpty : Boolean}]: Matcher[A] =
    BooleanPropertyMatcher("empty", _.isEmpty)

  // TODO: types(anyOf(1,2,3))
  // TODO: types(only(1,2,3))
  // TODO: types(inOrderOnly(1,2,3))
  // TODO: types(inOrder(1,2,3))

  def regex(x: Regex): Matcher[String] =
    FunMatcher(x.findFirstIn(_).isDefined, s"matches ${fmt(x)}", s"does not match ${fmt(x)}")

  def in(x: Regex, xs: Regex*): Matcher[String] = {
    val all = x +: xs
    inCombine(all, all.map(regex))
  }

  def in[A](x: A, xs: A*): Matcher[A] = {
    val all = x +: xs
    inCombine(all, all.map(is))
  }

  private def inCombine[A, B](items: Seq[B], all: Seq[Matcher[A]]): Matcher[A] = {

    new AbstractMatcher[A](s"is in ${fmtRight(items)}", s"is not in ${fmtRight(items)}") {
      def findMatching(v: A): Option[Matcher[A]] = all.find(_.matches(v).matches)

      override def test(v: A): Boolean = findMatching(v).isDefined

      override def describeMatch(v: A): Option[String] =
        findMatching(v).flatMap(_.describeMatch(v))
    }
  }

  def startsWith(x: String): Matcher[String] =
    FunMatcher(
      v => v.startsWith(x),
      s"starts with ${fmt(x)}",
      s"does not start with ${fmt(x)}"
    )

  // TODO: create a macro for this
  def size[A >: Traversable[_]](x: Matcher[Int], xs: Matcher[Int]*): Matcher[A] =
    PropertyMatcher[A, Int]("size", { v: Any => v.asInstanceOf[Iterable[_]].size }, x, xs)

  //
  //  def size[A, B <: {def size : A}] = new PropertyMatchHelper[B, A]("size", v => v.size)
}

object Matchers extends Matchers
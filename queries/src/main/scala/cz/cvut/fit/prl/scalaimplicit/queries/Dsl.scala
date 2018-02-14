package cz.cvut.fit.prl.scalaimplicit.queries

import cats._
import cats.implicits._

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.CallSite
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.Type

import scala.language.reflectiveCalls
import scala.language.implicitConversions

trait Matchers {

  case class MatchResult[A](matches: Boolean, matcher: Matcher[A]) {
    def unary_! = MatchResult(!matches, matcher)
  }

  trait Matcher[A] extends (A => MatchResult[A]) {
    def description: String

    def negativeDescription: String

    def describeMatch(v: A): Option[String]

    def describeMismatch(v: A): Option[String]

    //    def &&(m: Matcher[A]): Matcher[A] = new Matcher[A] {
    //      override def describeMismatch(v: A): String = ???
    //      override def describeMatch(v: A): String = ???
    //      override def apply(v: A): Boolean = Matcher.this(v) && m.apply(v)
    //    }
    //
    //    def ||(m: Matcher[A]): Matcher[A] = new Matcher[A] {
    //      override def describeMismatch(v: A): String = ???
    //      override def describeMatch(v: A): String = ???
    //      override def apply(v: A): Boolean = Matcher.this(v) || m.apply(v)
    //    }

    def unary_! = new Matcher[A] {
      override def description: String = Matcher.this.negativeDescription

      override def negativeDescription: String = Matcher.this.description

      override def apply(v: A): MatchResult[A] = !Matcher.this.apply(v)

      override def describeMatch(v: A): Option[String] = Matcher.this.describeMismatch(v)

      override def describeMismatch(v: A): Option[String] = Matcher.this.describeMatch(v)
    }
  }


  implicit def matcherSemigroup[A]: Semigroup[Matcher[A]] = (x: Matcher[A], y: Matcher[A]) => new Matcher[A] {
//    implicit val matcherDescriptionSemigroup: Semigroup[String] = (x: String, y: String) => x + "\n" + y

    override def description: String = x.description + "\n" + y.description

    override def negativeDescription: String = x.negativeDescription |+| y.negativeDescription

    override def describeMatch(v: A): Option[String] = (x.describeMatch(v), y.describeMatch(v)) match {
      case (Some(a), Some(b)) => Some(a |+| b)
      case (_, _) => None
    }

    override def describeMismatch(v: A): Option[String] = x.describeMismatch(v) |+| y.describeMismatch(v)

    override def apply(v: A): MatchResult[A] = x(v) |+| y(v)
  }

  implicit def matchResultSemigroup[A]: Semigroup[MatchResult[A]] = (x: MatchResult[A], y: MatchResult[A]) => {
    MatchResult(x.matches && y.matches, x.matcher |+| y.matcher)
  }

  class FunMatcher[A](fun: A => Boolean,
                      _description: String,
                      _negativeDescription: String,
                      _describeMatch: A => String,
                      _describeMismatch: A => String) extends Matcher[A] {
    override def description: String = _description

    override def negativeDescription: String = _negativeDescription

    override def describeMatch(v: A): Option[String] = {
      if (fun(v)) {
        Some(_describeMatch(v))
      } else {
        None
      }
    }

    override def describeMismatch(v: A): Option[String] = {
      if (!fun(v)) {
        Some(_describeMismatch(v))
      } else {
        None
      }
    }

    override def apply(v: A): MatchResult[A] = MatchResult(fun(v), this)
  }

  object FunMatcher {
    def apply[A](fun: A => Boolean, description: String, negativeDescription: String): FunMatcher[A] =
      new FunMatcher[A](
        fun,
        description,
        negativeDescription,
        v => s"`$v' $description",
        v => s"`$v' $negativeDescription"
      )
  }

  //
  //  class PropertyMatcher[A, B](val name: String, private val property: A => B, private val matcher: Matcher[B]) extends Matcher[A] {
  //    override def apply(v: A): Boolean = matcher(property(v))
  //    override def describeMismatch(v: A): String = ???
  //    override def describeMatch(v: A): String = ???
  //  }
  //
  //  // TODO: how to work with properties defined in multiple classes?
  //  // TODO: q"size > 5 && size < 6"
  //
  //  // size(is(5))
  //  // size == 5
  //  // size(5)
  def is[A](x: A): Matcher[A] = FunMatcher[A](v => v == x, s"== `$x'", s"!= `$x'")

  //
  //  // size(not(is(5))
  //  // size != 5
  def not[A](x: Matcher[A]): Matcher[A] = !x

  //
  //  def gt(x: Int): Matcher[Int] = new FunMatcher[Int](
  //    v => v > x,
  //    v => s"$v > $x",
  //    v => s"$v < $x"
  //  )
  //
  //  def lt(x: Int): Matcher[Int] = new FunMatcher[Int](
  //    v => v < x,
  //    v => s"$v < $x",
  //    v => s"$v > $x"
  //  )
  //
  //  def in(xs: String*): Matcher[String] = new FunMatcher[String](
  //    v => xs.contains(v),
  //    v => s"$v is in $xs",
  //    v => s"$v is not in $xs"
  //  )
  //
  //  implicit def int2matcher(x: Int): Matcher[Int] = is(x)
  //
  //  trait PropertyMatcherHelper[A, B] {
  //    def name: String
  //    def property(x: A): B
  //  }
  //
  //  trait IntPropertyMatcherHelper[A] extends PropertyMatcherHelper[A, Int] {
  //    def >(x: Int): Matcher[A] = new PropertyMatcher[A, Int](name, property, gt(x))
  //    def <(x: Int): Matcher[A] = new PropertyMatcher[A, Int](name, property, lt(x))
  //  }
  //
  //  trait EqPropertyMatcherHelper[A, B] extends PropertyMatcherHelper[A, B] {
  //    def ==(x: B): Matcher[A] = new PropertyMatcher[A, B](name, property, is(x))
  //    def !=(x: B): Matcher[A] = new PropertyMatcher[A, B](name, property, not(is(x)))
  //  }
  //
  //  class FunPropertyMatcherHelper[A, B](private val _name: String, private val _property: A => B) extends PropertyMatcherHelper[A, B] {
  //    def name = _name
  //    def property(x: A): B = _property(x)
  //  }
}

object Matchers extends Matchers

//trait RepresentationMatchers {
//  import Matchers._
//
//  type WithName = { def name: String }
//  type WithIntSize = { def size: Int }
//  type WithTypeArguments = { def typeArguments: Seq[Type] }
//
//  //def name[A <: WithName] = new StringMatcherBuilder[A](x => x.name)
//
//  def size[A <: WithIntSize] =
//    new FunPropertyMatcherHelper[A, Int]("size", x => x.size) with
//        IntPropertyMatcherHelper[A] with
//        EqPropertyMatcherHelper[A, Int]
//
//  def size[A <: WithIntSize](ms: Matcher[Int]*): Matcher[A] =
//    new PropertyMatcher[A, Int]("size", v => v.size, new ComposedMatcher[Int](ms))
//
//  def typeArguments[A <: WithTypeArguments](ms: Matcher[Seq[Type]]*): Matcher[A] =
//    new PropertyMatcher[A, Seq[Type]]("typeArguments", v => v.typeArguments, new ComposedMatcher[Seq[Type]](ms))
//
//  def typeArguments[A <: WithTypeArguments] =
//    new FunPropertyMatcherHelper[A, Seq[Type]]("typeArguments", v => v.typeArguments) with
//        SeqMatcherHelper[A, Seq[Type]]
//
//  //  typeArguments(size(5))
//  //  typeArguments(size > 5)
//
//  trait Matchable[A] {
//    def matches(ms: Matcher[A]): Boolean
//
//  }
//
//  implicit class AnyMatcher[A](that: A) extends Matchable[A] {
//    override def matches(ms: Matcher[A]): Boolean = ms(that)
//  }
//
//  implicit class CollectionQuery[A <: Matchable[A], B <: Traversable[A]](that: B) {
//    def query(ms: Matcher[A]) = that.filter(ms)
//  }
//}
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

package cz.cvut.fit.prl.scalaimplicit.queries

import cats._
import cats.implicits._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.CallSite
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.Type

import scala.language.reflectiveCalls
import scala.language.implicitConversions
import scala.runtime.ScalaNumberProxy

// TODO: or
// TODO: query | matches
// TODO: add result to MatchResult
// TODO: generate matchers for represenation
trait Matchers {

  trait ValueFormat[A] {
    def format(x: A): String
  }

  private val defaultValueFormatter: ValueFormat[Any] = x => x match {
    case x: String => "\"" + x + "\""
    case x: Char => s"'$x'"
    case _: Boolean | _:Byte | _:Short | _:Int | _:Long | _:Float | _:Double => s"$x"
    case _ => s"`$x'"
  }

  private def fmt[A](x: A)(implicit vf: ValueFormat[_ >: A] = defaultValueFormatter): String = vf.format(x)

  trait ImplicitValueFormats {
//    implicit val stringValueFormat: ValueFormat[String] = (x: String) => s"'$x'"
//    implicit val intValueFormat: ValueFormat[Int] = (x: Int) => s"$x"
//    implicit val booleanValueFormat: ValueFormat[Boolean] = (x: Boolean) => s"$x"
//    implicit val otherValueFormat: ValueFormat[Any] = (x: Any) => s"`$x'"
  }

  trait ImplicitMatchResult {
    implicit def matchResult2Boolean(that: MatchResult[_]): Boolean = that.matches
  }

  trait ImplicitMatchers {
    implicit class AnyMatcher[A](that: A) {
      def matches(ms: Matcher[A]): MatchResult[A] = ms(that)
    }

    implicit def any2matcher[A](x: A): Matcher[A] = is(x)
  }

  object implicits extends ImplicitValueFormats with ImplicitMatchResult with ImplicitMatchers

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

  // TODO: isn't this in cats already?
  private def reduce[A: Semigroup](x: A, xs: Seq[A]): A = xs match {
    case Seq() => x
    case Seq(y, ys@_*) => reduce(x |+| y, ys)
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
        v => s"${fmt(v)} $description",
        v => s"${fmt(v)} $negativeDescription"
      )
  }

  class PropertyMatcher[A, B](val name: String, private val property: A => B, private val matcher: Matcher[B]) extends Matcher[A] {

    override def description: String = name + " " + matcher.description

    override def negativeDescription: String = name + " " + matcher.negativeDescription

    override def describeMatch(v: A): Option[String] = matcher.describeMatch(property(v)).map(name + " " + _)

    override def describeMismatch(v: A): Option[String] = matcher.describeMismatch(property(v)).map(name + " " + _)

    override def apply(v: A): MatchResult[A] = MatchResult(matcher(property(v)).matches, this)
  }

  object PropertyMatcher {
    def apply[A, B](name: String, property: A => B, matcher: Matcher[B], matchers: Seq[Matcher[B]]) =
      new PropertyMatcher[A, B](name, property, reduce(matcher, matchers))
  }

  class PropertyMatchHelper[A, B](val name: String, val property: A => B) {
    def ==(x: B): Matcher[A] = new PropertyMatcher[A, B](name, property, is(x))
    def !=(x: B): Matcher[A] = new PropertyMatcher[A, B](name, property, not(is(x)))
  }

  trait OrderingPropertyMatchHelper[A, B] extends PropertyMatchHelper[A, B] {
    def >(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, gt(x))
    def <(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, lt(x))
    def >=(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, gteq(x))
    def <=(x: B)(implicit ev: Ordering[B]): Matcher[A] = new PropertyMatcher[A, B](name, property, lteq(x))
  }

  //  class FunPropertyMatcherHelper[A, B](private val _name: String, private val _property: A => B) extends PropertyMatcherHelper[A, B] {
  //    def name = _name
  //    def property(x: A): B = _property(x)
  //  }


  def is[A](x: A): Matcher[A] = FunMatcher[A](v => v == x, s"== ${fmt(x)}", s"!= ${fmt(x)}")
  def not[A](x: Matcher[A]): Matcher[A] = !x

  def gt[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].gt(v, x), s"> ${fmt(x)}", s"< ${fmt(x)}")
  def lt[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].lt(v, x), s"< ${fmt(x)}", s"> ${fmt(x)}")
  def gteq[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].gteq(v, x), s">= ${fmt(x)}", s"<= ${fmt(x)}")
  def lteq[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].lteq(v, x), s"<= ${fmt(x)}", s">= ${fmt(x)}")

  //  def size[A <: WithIntSize] =
  //    new FunPropertyMatcherHelper[A, Int]("size", x => x.size) with
  //        IntPropertyMatcherHelper[A] with
  //        EqPropertyMatcherHelper[A, Int]
  //

  def in[A](xs: A*): Matcher[A] = FunMatcher[A](
    v => xs.contains(v),
    "in " + xs.map(x => fmt(x)).mkString("{", ",", "}"),
    "not in " + xs.map(x => fmt(x)).mkString("{", ",", "}")
  )

  // TODO: can we create a macro for this
  def size[A, B <: {def size: A}](x: Matcher[A], xs: Matcher[A]*): Matcher[B] =
    PropertyMatcher[B, A]("size", v => v.size, x, xs)

  def size[A, B <: {def size: A}] = new PropertyMatchHelper[B, A]("size", v => v.size)
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

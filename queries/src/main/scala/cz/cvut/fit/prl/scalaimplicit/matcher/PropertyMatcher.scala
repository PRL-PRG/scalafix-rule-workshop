package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.matcher.EqMatchers._

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
  def apply[A, B](name: String, property: A => B, matcher: Matcher[B], matchers: Seq[Matcher[B]]): Matcher[A] = {
    val combined = (matcher +: matchers) reduce (combineAnd(_, _))
    new PropertyMatcher[A, B](name, property, combined)
  }

  def apply[A, B](name: String, matcher: Matcher[B], matchers: Seq[Matcher[B]])(implicit pg: PG[A, _, B]): Matcher[A] =
    apply(name, pg.get, matcher, matchers)
}

object BooleanPropertyMatcher {
  // TODO: extract class
  def apply[A](name: String, property: A => Boolean): Matcher[A] =
    new PropertyMatcher[A, Boolean](name, property, is(true)) {
      override def description: String = s"is $name"

      override def negativeDescription: String = s"is not $name"

      override def describeMatch(v: A): Option[String] =
        if (test(v)) Some(s"$v is $name") else None

      override def describeMismatch(v: A): Option[String] =
        if (!test(v)) Some(s"$v is not $name") else None
    }

  def apply[A](name: String)(implicit pg: PG[A, _, Boolean]): Matcher[A] =
    apply(name, pg.get)
}

// TODO: implement
class OptionPropertyMatcher[A, B](f: A => Option[B], x: Matcher[B], xs: Seq[Matcher[B]]) extends Matcher[A] {
  val combined = (x +: xs) reduce (combineAnd(_, _))

  override def test(v: A): Boolean = f(v).exists(combined.test)

  override def description: String = "contains " + combined.description

  override def negativeDescription: String = ???

  override def describeMatch(v: A): Option[String] = ???

  override def describeMismatch(v: A): Option[String] = {
    f(v).flatMap(combined.describeMismatch).orElse(Some("None " + combined.negativeDescription))
  }

}


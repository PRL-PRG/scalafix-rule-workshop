package cz.cvut.fit.prl.scalaimplicit.matcher

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

object MatchResult {
  def apply[A](matches: Boolean, instance: A, matcher: Matcher[A]): MatchResult[A] =
    if (matches) Match(instance, matcher) else Mismatch(instance, matcher)
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

  def describeMatch(v: A): Option[String] = if (test(v)) Some(description) else None

  def describeMismatch(v: A): Option[String] = if (!test(v)) Some(s"(${fmt(v)}) $negativeDescription") else None

  def matches[A1 <: A](v: A1): MatchResult[A1] = MatchResult(test(v), v, this)

  // TODO: will it be possible to define &&
  // TODO: will it be possible to define ||

  def unary_!(): Matcher[A] = new Matcher[A] {
    override def test(v: A): Boolean = !self.test(v)

    override def description: String = self.negativeDescription

    override def negativeDescription: String = self.description

    override def describeMatch(v: A): Option[String] = self.describeMismatch(v)

    override def describeMismatch(v: A): Option[String] = self.describeMatch(v)
  }

  override def toString: String = description
}

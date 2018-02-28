package cz.cvut.fit.prl.scalaimplicit.queries

import cats.implicits._

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
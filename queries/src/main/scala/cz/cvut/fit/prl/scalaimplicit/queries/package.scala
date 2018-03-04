package cz.cvut.fit.prl.scalaimplicit

import cats.kernel.Semigroup

import scala.util.matching.Regex

package object queries {

  object implicits extends ImplicitMatchers

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

  //  // TODO: isn't this in cats already?
  //  private def reduce[A](x: Matcher[A], xs: Seq[Matcher[A]])(f: (Matcher[A], Matcher[A]) => Matcher[A]): Matcher[A] =
  //    xs match {
  //      case Seq() => x
  //      case Seq(y, ys@_*) => reduce(f(x, y), ys)(f)
  //    }

  abstract class AbstractMatcher[-A](val description: String, val negativeDescription: String) extends Matcher[A] {
    override def describeMatch(v: A): Option[String] = {
      if (test(v)) {
        Some(description)
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

  def combineIn[A, B](items: Seq[B], all: Seq[Matcher[A]]): Matcher[A] =
    new AbstractMatcher[A](s"is in ${fmtRight(items)}", s"is not in ${fmtRight(items)}") {
      def findMatching(v: A): Option[Matcher[A]] = all.find(_.matches(v).matches)

      override def test(v: A): Boolean = findMatching(v).isDefined

      override def describeMatch(v: A): Option[String] =
        findMatching(v).flatMap(_.describeMatch(v))
    }

  def combineAnd[A](m1: Matcher[A], m2: Matcher[A]): Matcher[A] = new Matcher[A] {
    override def test(v: A): Boolean = m1.test(v) && m2.test(v)

    override def description: String = m1.description + " && " + m2.description

    override def negativeDescription: String = m1.negativeDescription + " || " + m2.negativeDescription

    override def describeMatch(v: A): Option[String] =
      for {
        d1 <- m1.describeMatch(v)
        d2 <- m2.describeMatch(v)
      } yield d1 + " && " + d2

    override def describeMismatch(v: A): Option[String] = (m1.describeMismatch(v), m2.describeMismatch(v)) match {
      case (Some(m1), Some(m2)) => Some(s"($m1) && ($m2)")
      case (m1@Some(_), None) => m1
      case (None, m2@Some(_)) => m2
      case _ => None
    }
  }

  def combineOr[A](m1: Matcher[A], m2: Matcher[A]): Matcher[A] = new Matcher[A] {
    override def test(v: A): Boolean = m1.test(v) || m2.test(v)

    override def description: String = m1.description + " || " + m2.description

    override def negativeDescription: String = m1.negativeDescription + " && " + m2.negativeDescription

    override def describeMatch(v: A): Option[String] = m1.matches(v) match {
      case result if result.matches => m1.describeMatch(v)
      case _ => m2.describeMatch(v)
    }

    override def describeMismatch(v: A): Option[String] = (m1.describeMismatch(v), m2.describeMismatch(v)) match {
      case (Some(m1), Some(m2)) => Some(s"($m1) && ($m2)")
      case (m1@Some(_), None) => m1
      case _ => None
    }
  }

  def and[A](x1: Matcher[A], x2: Matcher[A], xs: Matcher[A]*): Matcher[A] =
    (x1 +: x2 +: xs) reduce (combineAnd(_, _))

  def or[A](x1: Matcher[A], x2: Matcher[A], xs: Matcher[A]*): Matcher[A] =
    (x1 +: x2 +: xs) reduce (combineOr(_, _))
}

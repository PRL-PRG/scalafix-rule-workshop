package cz.cvut.fit.prl.scalaimplicit

import scala.util.matching.Regex

package object matcher {

  // TODO: make it a default instance of some Pretifier
  def fmt[A](s: A): String = s match {
    case x: Regex => "Regex(\"" + x + "\")"
    case x: String => "\"" + x + "\""
    case x: Char => s"'$x'"
    case x@(_: Boolean | _: Byte | _: Short | _: Int | _: Long | _: Float | _: Double) => s"$x"
    //    case x: Map[_, _] => x.map { case (k, v) => fmt(k) + ": " + fmt(v) }.mkString("{", ", ", "}")
    case x => x.toString
  }

  def fmtAsList(xs: Traversable[String]): String = xs mkString("[", ", ", "]")

  def fmtAsMap(xs: Traversable[String]): String = xs mkString("{", ", ", "}")

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
        Some(s"(${fmt(v)}) $negativeDescription")
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

}

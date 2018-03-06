package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.matcher.EqMatchers._

import scala.util.matching.Regex

trait StringMatchers {
  def regex(x: Regex): Matcher[String] =
    FunMatcher(x.findFirstIn(_).isDefined, s"matches ${fmt(x)}", s"does not match ${fmt(x)}")

  def in(x: Regex, xs: Regex*): Matcher[String] = combineIn((x +: xs).map(regex))

  def startsWith(x: String): Matcher[String] =
    FunMatcher(
      v => v.startsWith(x),
      s"starts with ${fmt(x)}",
      s"does not start with ${fmt(x)}"
    )
}

object StringMatchers extends StringMatchers

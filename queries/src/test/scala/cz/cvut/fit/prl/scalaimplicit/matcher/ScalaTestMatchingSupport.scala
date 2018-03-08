package cz.cvut.fit.prl.scalaimplicit.matcher

import org.scalatest.{Matchers => ScalaTestMatchers}
import org.scalatest.matchers.{MatchResult => ScalaTestMatchResult, Matcher => ScalaTestMatcher}

trait ScalaTestMatchingSupport {
  private def test(matches: Boolean, reason: Option[String]) = new ScalaTestMatcher[MatchResult[_]] {
    def apply(left: MatchResult[_]): ScalaTestMatchResult = {
      if (left.matches == matches) {
        val reasonCheck =
          reason.map(ScalaTestMatchers.be(_).apply(left.reason)).getOrElse(ScalaTestMatchResult(true, "", ""))

        ScalaTestMatchResult(
          matches = reasonCheck.matches,
          s"${left.instance} ${if (matches) "" else "mis"}matched but with a different reason: ${reasonCheck.failureMessage}",
          s"${left.instance} ${if (matches) "mis" else ""}matched: ${left.reason} (matcher: ${left.matcher})"
        )
      } else {
        ScalaTestMatchResult(
          matches = false,
          s"${left.instance} ${if (matches) "mis" else ""}matched: ${left.reason} (matcher: ${left.matcher})",
          s"${left.instance} ${if (matches) "" else "mis"}matched: ${left.reason} (matcher: ${left.matcher})"
        )
      }
    }
  }

  def matched(reason: String): ScalaTestMatcher[MatchResult[_]] = test(true, Some(reason))

  def matched: ScalaTestMatcher[MatchResult[_]] = test(true, None)

  def mismatched(reason: String): ScalaTestMatcher[MatchResult[_]] = test(false, Some(reason))

  def mismatched: ScalaTestMatcher[MatchResult[_]] = test(false, None)
}

object ScalaTestMatchingSupport extends ScalaTestMatchingSupport

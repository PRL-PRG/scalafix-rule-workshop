package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.matcher.LogicalMatchers._

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

trait ImplicitMatchers {

  // TODO: numbers, strings, bools
  // implicit def any2matcher[A](x: A): Matcher[A] = is(x)

  implicit class QueryCollectionSupport[A, +Repr](
      that: TraversableLike[A, Repr]) {

    def query[That](matcher: Matcher[A], matchers: Matcher[A]*)(
        implicit bf: CanBuildFrom[Repr, MatchResult[A], That]): That =
      that.map(combineAnd(matcher +: matchers).matches)

    def select(matcher: Matcher[A], matchers: Matcher[A]*): Repr =
      that.filter(combineAnd(matcher +: matchers).matches(_).matches)
  }

  implicit class AnyMatcher[A](that: A) {
    def matches(matcher: Matcher[A]): MatchResult[A] =
      matcher.matches(that)

    def matches(matcher1: Matcher[A],
                matcher2: Matcher[A],
                matchers: Matcher[A]*): MatchResult[A] =
      matches(combineAnd(matcher1 +: matcher2 +: matchers))
  }

}

object ImplicitMatchers extends ImplicitMatchers

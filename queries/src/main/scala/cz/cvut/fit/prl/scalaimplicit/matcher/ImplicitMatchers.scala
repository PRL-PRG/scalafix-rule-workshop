package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.matcher.LogicalMatchers._

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

trait ImplicitMatchers {

  // TODO: numbers, strings, bools
  // implicit def any2matcher[A](x: A): Matcher[A] = is(x)

  implicit class QueryCollectionSupport[A, +Repr](that: TraversableLike[A, Repr]) {
    def query[That](matcher: Matcher[A], matchers: Matcher[A]*)(implicit bf: CanBuildFrom[Repr, MatchResult[A], That]): That =
      that.map(combineAnd(matcher +: matchers).matches)
  }

  implicit class AnyMatcher[A](that: A) {
    def matches(matcher: Matcher[A]): MatchResult[A] = matcher.matches(that)
  }

}

object ImplicitMatchers extends ImplicitMatchers

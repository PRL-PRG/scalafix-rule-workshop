package cz.cvut.fit.prl.scalaimplicit.matcher

trait EqMatchers {
  def is[A](x: A): Matcher[A] =
    FunMatcher[A](v => v == x, s"is ${fmt(x)}", s"is not ${fmt(x)}")

  def not[A](x: Matcher[A]): Matcher[A] = !x

  def in[A](x: A, xs: A*): Matcher[A] = {
    // TODO: there is a duplication with IterableMatches.allOf
    def is2(x: A) = FunMatcher[A](v => v == x, fmt(x), fmt(x))

    combineIn((x +: xs) map is2)
  }

  def combineIn[A](xs: Seq[Matcher[A]]): Matcher[A] = {
    val itemsDescription = fmtAsList(xs.map(fmt))

    new AbstractMatcher[A](s"is in $itemsDescription", s"is not in $itemsDescription") {
      def findMatching(v: A): Option[Matcher[A]] = xs.find(_.matches(v).matches)

      override def test(v: A): Boolean = findMatching(v).isDefined

      override def describeMatch(v: A): Option[String] =
        findMatching(v).flatMap(_.describeMatch(v))
    }
  }

  def isNone[A]: Matcher[Option[A]] = FunMatcher(_.isEmpty, "is None", "is Some")

  def some[A](x: Matcher[A]): Matcher[Option[A]] = new Matcher[Option[A]] {
    override def test(v: Option[A]): Boolean = v.exists(x.test)

    override def description: String = "value " + x.description

    override def describeMismatch(v: Option[A]): Option[String] =
      v.flatMap(x.describeMismatch).orElse(Some("empty value " + x.negativeDescription))

    override def negativeDescription: String = "value " + x.negativeDescription

    override def describeMatch(v: Option[A]): Option[String] =
      v.flatMap(x.describeMatch)
  }

  def mismatch[A](reason: String): Matcher[A] = new Matcher[A] {
    override def test(v: A): Boolean = false

    override def description: String = s"always mismatch: $reason"

    override def describeMismatch(v: A): Option[String] = Some(s"${fmt(v)} $reason")

    override def negativeDescription: String = "always match: $reason"

    override def describeMatch(v: A): Option[String] = None
  }

}

object EqMatchers extends EqMatchers

package cz.cvut.fit.prl.scalaimplicit.matcher

trait EqMatchers {
  def is[A](x: A): Matcher[A] =
    FunMatcher[A](v => v == x, s"is ${fmt(x)}", s"is not ${fmt(x)}")

  def not[A](x: Matcher[A]): Matcher[A] = !x

  case class in[A](x: A, xs: A*) extends Matcher[A] {
    private val all = x +: xs
    private val itemsDescription = fmtAsList(all.map(fmt))

    private def findMatching(v: A): Option[A] = all.find(_ == v)

    override val description = s"is in $itemsDescription"

    override val negativeDescription = s"is not in $itemsDescription"

    override def test(v: A): Boolean = findMatching(v).isDefined

    override def describeMatch(v: A): Option[String] = findMatching(v).map("is " + fmt(_))
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

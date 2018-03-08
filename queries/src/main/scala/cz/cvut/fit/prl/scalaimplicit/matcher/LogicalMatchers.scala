package cz.cvut.fit.prl.scalaimplicit.matcher

trait LogicalMatchers {

  import LogicalMatchers._

  case class and[A](m1: Matcher[A], m2: Matcher[A]) extends Matcher[A] {
    def this(x1: Matcher[A], x2: Matcher[A], xs: Matcher[A]*) = this(x1, and(x2, xs reduce and[A]))

    override def test(v: A): Boolean = m1.test(v) && m2.test(v)

    override def description: String = concat(m1.description, "&&", m2.description)

    override def negativeDescription: String = concat(m1.negativeDescription, "||", m2.negativeDescription)

    override def describeMatch(v: A): Option[String] =
      for {
        d1 <- m1.describeMatch(v)
        d2 <- m2.describeMatch(v)
      } yield concat(d1, "&&", d2)

    override def describeMismatch(v: A): Option[String] = (m1.describeMismatch(v), m2.describeMismatch(v)) match {
      case (Some(d1), Some(d2)) => Some(concat(d1, "&&", d2))
      case (d1@Some(_), None) => d1
      case (None, d2@Some(_)) => d2
      case _ => None
    }
  }

  case class or[A](m1: Matcher[A], m2: Matcher[A]) extends Matcher[A] {
    def this(x1: Matcher[A], x2: Matcher[A], xs: Matcher[A]*) = this(x1, or(x2, xs reduce or[A]))

    override def test(v: A): Boolean = m1.test(v) || m2.test(v)

    override def description: String = concat(m1.description, "||", m2.description)

    override def negativeDescription: String = concat(m1.negativeDescription, "&&", m2.negativeDescription)

    override def describeMatch(v: A): Option[String] = m1.matches(v) match {
      case result if result.matches => m1.describeMatch(v)
      case _ => m2.describeMatch(v)
    }

    override def describeMismatch(v: A): Option[String] = (m1.describeMismatch(v), m2.describeMismatch(v)) match {
      case (Some(d1), Some(d2)) => Some(concat(d1, "&&", d2))
      case (d1@Some(_), None) => d1
      case _ => None
    }
  }

}

object LogicalMatchers extends LogicalMatchers {
  private def concat(m1: String, c: String, m2: String) =
    s"($m1) $c ($m2)"

  def combineAnd[A](matchers: Seq[Matcher[A]]): Matcher[A] = matchers reduce and[A]

  def combineOr[A](matchers: Seq[Matcher[A]]): Matcher[A] = matchers reduce or[A]
}

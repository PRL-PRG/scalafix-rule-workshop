package cz.cvut.fit.prl.scalaimplicit.queries

trait OrderingMatchers {
  def gt[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].gt(v, x), s"is > ${fmt(x)}", s"is < ${fmt(x)}")

  def lt[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].lt(v, x), s"is < ${fmt(x)}", s"is > ${fmt(x)}")

  def gteq[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].gteq(v, x), s"is >= ${fmt(x)}", s"is <= ${fmt(x)}")

  def lteq[A: Ordering](x: A): Matcher[A] =
    FunMatcher[A](v => implicitly[Ordering[A]].lteq(v, x), s"is <= ${fmt(x)}", s"is >= ${fmt(x)}")
}

object OrderingMatchers extends OrderingMatchers
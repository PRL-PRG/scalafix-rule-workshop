package cz.cvut.fit.prl.scalaimplicit.queries

trait EqMatchers {
  def is[A](x: A): Matcher[A] =
    FunMatcher[A](v => v == x, s"is ${fmt(x)}", s"is not ${fmt(x)}")

  def not[A](x: Matcher[A]): Matcher[A] = !x

  def in[A](x: A, xs: A*): Matcher[A] = {
    val all = x +: xs
    inCombine(all, all.map(is))
  }
}

object EqMatchers extends EqMatchers

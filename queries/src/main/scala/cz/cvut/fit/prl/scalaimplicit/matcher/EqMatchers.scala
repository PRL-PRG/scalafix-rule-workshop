package cz.cvut.fit.prl.scalaimplicit.matcher

trait EqMatchers {
  def is[A](x: A): Matcher[A] =
    FunMatcher[A](v => v == x, s"is ${fmt(x)}", s"is not ${fmt(x)}")

  def not[A](x: Matcher[A]): Matcher[A] = !x

  // TODO: make consistent with the other combinators
  def in[A](x: A, xs: A*): Matcher[A] = {
    val all = x +: xs
    combineIn(all, all.map(is))
  }
}

object EqMatchers extends EqMatchers

package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.queries.EqMatchers._

trait IterableMatchers {

  def contains[A](x: Matcher[A], xs: Matcher[A]*): Matcher[Iterable[A]] = {
    val all = x +: xs
    val items = all map (_.description)

    new AbstractMatcher[Iterable[A]](s"contains ${fmtRight(items)}", s"does not contain ${fmtRight(items)}") {
      def missing(v: Iterable[A]): Seq[Matcher[A]] = all.filterNot(y => v.exists(y.test))

      override def test(v: Iterable[A]): Boolean = missing(v).isEmpty

      override def describeMismatch(v: Iterable[A]): Option[String] =
      // TODO: do not use fmtRight
        if (!test(v)) Some(s"${fmt(v)} does not contain ${fmtRight(missing(v) map (_.description))}") else None
    }
  }

  // TODO: this should use combineIn - message shoud have [x, ...] and 'is missing'
  def allOf[A1 >: A, A](x: A1, xs: A1*): Matcher[Iterable[A]] =
    contains[A1](is(x), xs.map(is): _*)

  // TODO: types(anyOf(1,2,3))
  // TODO: types(only(1,2,3))
  // TODO: types(inOrderOnly(1,2,3))
  // TODO: types(inOrder(1,2,3))

  def isEmpty[A]: Matcher[Iterable[A]] = FunMatcher(v => v.isEmpty, "is empty", "is not empty")

  def size[A](x: Matcher[Int]): Matcher[Iterable[A]] =
    FunMatcher(v => x.test(v.size), s"size ${x.description}", s"size ${x.negativeDescription}")

  def size[A](x: Int): Matcher[Iterable[A]] = size(is(x))
}

object IterableMatchers extends IterableMatchers
package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.matcher.EqMatchers._
import cz.cvut.fit.prl.scalaimplicit.matcher.LogicalMatchers._

import scala.language.reflectiveCalls

trait IterableMatchers {

  def contains[A](x: Matcher[A], xs: Matcher[A]*): Matcher[Iterable[A]] = {
    val all = x +: xs
    val items = all map (_.description)

    new AbstractMatcher[Iterable[A]](s"contains ${fmtAsList(items)}", s"does not contain ${fmtAsList(items)}") {
      def missing(v: Iterable[A]): Seq[Matcher[A]] = all.filterNot(y => v.exists(y.test))

      override def test(v: Iterable[A]): Boolean = missing(v).isEmpty

      override def describeMismatch(v: Iterable[A]): Option[String] = {
        // TODO: avoid calling it twice
        val missingDescription = fmtAsList(missing(v) map (_.description))
        if (!test(v)) Some(s"${fmt(v)} is missing $missingDescription") else None
      }
    }
  }

  def inOrderOnly[A](x: Matcher[A], xs: Matcher[A]*): Matcher[Iterable[A]] = {
    val all = x +: xs
    val items = all map (_.description)

    new AbstractMatcher[Iterable[A]](s"contains in order only ${fmtAsList(items)}", s"does not contain in order ${fmtAsList(items)}") {

      def problems(v: Iterable[A]): Seq[(Int, String)] = {
        val sdMatchersElems = Math.max(all.size - v.size, 0)
        val extendedElements = v.map(Option.apply) ++ ((0 until sdMatchersElems) map (_ => None))

        val sdElemsMatchers = Math.max(v.size - all.size, 0)
        val extendedMatchers =
          (all ++ (0 until sdElemsMatchers).map(_ => mismatch("should be empty"))).map(some[A])

        (extendedMatchers zip extendedElements)
          .map {
            case (m, elem) => m.matches(elem)
          }
          .zipWithIndex
          .collect {
            case (r@Mismatch(_, _), idx) => idx -> r.reason
          }
      }

      override def test(v: Iterable[A]): Boolean = problems(v).isEmpty

      override def describeMismatch(v: Iterable[A]): Option[String] = {
        // TODO: avoid calling it twice
        val problemsDescription = fmtAsMap(problems(v) map { case (idx, r) => s"$idx: $r" })
        if (!test(v)) Some(s"${fmt(v)} is missing in order $problemsDescription") else None
      }
    }

  }

  def allOf[A1 >: A, A](x: A1, xs: A1*): Matcher[Iterable[A]] = {
    // this is like is but without the 'is'/'is not' in description
    def is2(x: A1) = FunMatcher[A1](v => v == x, fmt(x), fmt(x))

    contains[A1](is2(x), xs.map(is2): _*)
  }

  // TODO: anyOf(1,2,3)

  def isEmpty[A <: {def isEmpty : Boolean}]: Matcher[A] =
    BooleanPropertyMatcher("empty", _.isEmpty)

  def size[A <: {def size : Int}](x: Matcher[Int], xs: Matcher[Int]*): PropertyMatcher[A, Int] =
    PropertyMatcher[A, Int]("size", _.size, combineAnd(x +: xs))

  def size[A <: {def size : Int}](x: Int): Matcher[A] = size(is(x))
}

object IterableMatchers extends IterableMatchers
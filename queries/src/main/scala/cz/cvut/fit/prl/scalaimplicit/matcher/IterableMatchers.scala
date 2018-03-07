package cz.cvut.fit.prl.scalaimplicit.matcher

import cz.cvut.fit.prl.scalaimplicit.matcher.EqMatchers._
import cz.cvut.fit.prl.scalaimplicit.matcher.LogicalMatchers._

import scala.language.reflectiveCalls

trait IterableMatchers {

  case class contains[-A](x: Matcher[A], xs: Matcher[A]*) extends Matcher[Iterable[A]] {
    private val all = x +: xs
    private val items = all map (_.description)

    override val description: String = s"contains ${fmtAsList(items)}"
    override val negativeDescription: String = s"does not contain ${fmtAsList(items)}"

    private def doTest(v: Iterable[A]) = all.filterNot(y => v.exists(y.test))

    override def test(v: Iterable[A]): Boolean = doTest(v).isEmpty

    override def describeMismatch(v: Iterable[A]): Option[String] = doTest(v) match {
      case Seq() => None
      case Seq(missing@_*) => Some(s"${fmt(v)} is missing ${fmtAsList(missing map (_.description))}")
    }
  }


  case class inOrderOnly[A](x: Matcher[A], xs: Matcher[A]*) extends Matcher[Iterable[A]] {
    private val all = x +: xs
    private val items = all map (_.description)

    private def doTest(v: Iterable[A]): Seq[(Int, String)] = {
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

    override val description = s"contains in order only ${fmtAsList(items)}"

    override val negativeDescription = s"does not contain in order ${fmtAsList(items)}"

    override def test(v: Iterable[A]): Boolean = doTest(v).isEmpty

    override def describeMismatch(v: Iterable[A]): Option[String] = doTest(v) match {
      case Seq() => None
      case Seq(nonMatching@_*) => {
        val problems = fmtAsMap(nonMatching map { case (idx, r) => s"$idx: $r" })
        Some(s"${fmt(v)} not matching on indices $problems")
      }
    }
  }

  def allOf[A1 >: A, A](x: A1, xs: A1*): Matcher[Iterable[A]] = {
    // this is like is but without the 'is'/'is not' in description
    def is2(x: A1) = FunMatcher[A1](v => v == x, fmt(x), fmt(x))

    contains[A1](is2(x), xs.map(is2): _*)
  }

  // TODO: anyOf(1,2,3) which is basically like in - anyOf(x, xs) => contains(in(x, xs))?

  def isEmpty[A <: {def isEmpty : Boolean}]: Matcher[A] =
    BooleanPropertyMatcher("empty", _.isEmpty)

  def size[A <: {def size : Int}](x: Matcher[Int], xs: Matcher[Int]*): PropertyMatcher[A, Int] =
    PropertyMatcher[A, Int]("size", _.size, combineAnd(x +: xs))

  def size[A <: {def size : Int}](x: Int): Matcher[A] = size(is(x))
}

object IterableMatchers extends IterableMatchers
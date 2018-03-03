package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.Declaration

import scala.language.higherKinds

trait ContainerMatchers {

  def contains[A, B <: Iterable[A]](x: Matcher[A]): Matcher[B] =
    new FunMatcher[B](v => v.exists(x.test), s"contains ${x.description}", s"does not contain ${x.description}")

  //def allOf[A, A1 >: A, B1 <: Iterable[A1]](x: A, xs: A*): Matcher[B1] = {
  def allOf[A, B1 <: Iterable[_ >: A]](x: A, xs: A*): Matcher[B1] = {
    val all = x +: xs

    new AbstractMatcher[B1](s"contains all of ${fmtRight(all)}", s"does not contain all of ${fmtRight(all)}") {
      def missing(v: B1): Seq[A] = Seq() //all.filterNot(y => v.exists{z : Any => z == y})

      override def test(v: B1): Boolean = missing(v).isEmpty

      override def describeMismatch(v: B1): Option[String] =
        if (!test(v)) Some(s"${fmt(v)} is missing ${fmtRight(missing(v))}") else None
    }
  }

  //  // matchers for collections
  //  def contains[A, B](x: Matcher[A]): Matcher[B] = {
  //    new AbstractMatcher[B](s"contains ${x.description}", s"does not contain ${x.description}") {
  //      def exists(v: B, p: A => Boolean): Boolean = v match {
  //        case v: Option[A] => v.exists(p)
  //        case v: Seq[A] => v.exists(p)
  //        case v: Traversable[A] => v.exists(p)
  //        case _ => throw new IllegalArgumentException(s"${v.getClass} is not supported")
  //      }
  //
  //      override def test(v: B): Boolean = exists(v, x.test)
  //    }
  //  }
  //
  //  // TODO: the B should be constrained, but I did not figure out any reasonable way
  //  // that will work with different containers (e.g. Option, Traversable, ...)
  //  // Ideally there should be a type class that would provide a the needed
  //  // functionality
  //  def allOf[A, B](x: A, xs: A*): Matcher[B] = {
  //    val all = x +: xs
  //
  //    new AbstractMatcher[B](s"contains all of ${fmtRight(all)}", s"does not contain all of ${fmtRight(all)}") {
  //      def contains(v: B, e: A): Boolean = v match {
  //        case v: Option[A] => v.contains(e)
  //        case v: Seq[A] => v.contains(e)
  //        case v: Traversable[A] => v.exists(_ == e)
  //        case _ => throw new IllegalArgumentException(s"${v.getClass} is not supported")
  //      }
  //
  //      def missing(v: B): Seq[A] = all.filterNot(e => contains(v, e))
  //
  //      override def test(v: B): Boolean = missing(v).isEmpty
  //
  //      override def describeMismatch(v: B): Option[String] =
  //        if (!test(v)) Some(s"${fmt(v)} is missing ${fmtRight(missing(v))}") else None
  //    }
  //  }

  // TODO: types(anyOf(1,2,3))
  // TODO: types(only(1,2,3))
  // TODO: types(inOrderOnly(1,2,3))
  // TODO: types(inOrder(1,2,3))

  trait PSize

  implicit def PGSize[A <: {def size : Int}]: PG[A, PSize, Int] = PG(_.size)

  def isEmpty[B](implicit pg: PG[B, PSize, Int]): Matcher[B] =
    new FunMatcher[B](v => pg.get(v) == 0, "is empty", "is not empty")

  def size[B](x: Matcher[Int])(implicit pg: PG[B, PSize, Int]): Matcher[B] =
    FunMatcher(v => x.test(pg.get(v)), s"size ${x.description}", s"size ${x.negativeDescription}")
}

object ContainerMatchers extends ContainerMatchers
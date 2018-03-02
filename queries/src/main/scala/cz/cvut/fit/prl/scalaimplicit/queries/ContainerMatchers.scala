package cz.cvut.fit.prl.scalaimplicit.queries

trait ContainerMatchers {

  // matchers for collections
  def contains[A, B](x: Matcher[A]): Matcher[B] = {
    new AbstractMatcher[B](s"contains ${x.description}", s"does not contain ${x.description}") {
      // TODO: B should be constrained
      def exists(v: B, p: A => Boolean): Boolean = v match {
        case v: Option[A] => v.exists(p)
        case v: Seq[A] => v.exists(p)
        case v: Traversable[A] => v.exists(p)
        case _ => throw new IllegalArgumentException(s"${v.getClass} is not supported")
      }

      override def test(v: B): Boolean = exists(v, x.test)
    }
  }

  // TODO: the B should be constrained, but I did not figure out any reasonable way
  // that will work with different containers (e.g. Option, Traversable, ...)
  // Ideally there should be a type class that would provide a the needed
  // functionality
  def allOf[A, B](x: A, xs: A*): Matcher[B] = {
    val all = x +: xs

    new AbstractMatcher[B](s"contains all of ${fmtRight(all)}", s"does not contain all of ${fmtRight(all)}") {
      def contains(v: B, e: A): Boolean = v match {
        case v: Option[A] => v.contains(e)
        case v: Seq[A] => v.contains(e)
        case v: Traversable[A] => v.exists(_ == e)
        case _ => throw new IllegalArgumentException(s"${v.getClass} is not supported")
      }

      def missing(v: B): Seq[A] = all.filterNot(e => contains(v, e))

      override def test(v: B): Boolean = missing(v).isEmpty

      override def describeMismatch(v: B): Option[String] =
        if (!test(v)) Some(s"${fmt(v)} is missing ${fmtRight(missing(v))}") else None
    }
  }

  def isEmpty[A <: {def isEmpty : Boolean}]: Matcher[A] =
    BooleanPropertyMatcher("empty", _.isEmpty)

  // TODO: types(anyOf(1,2,3))
  // TODO: types(only(1,2,3))
  // TODO: types(inOrderOnly(1,2,3))
  // TODO: types(inOrder(1,2,3))

  def size[A >: Traversable[_]](x: Matcher[Int]): Matcher[A] =
    new AbstractMatcher[A](s"size ${x.description}", s"size ${x.negativeDescription}") {
      def size(v: A) = v match {
        case v: Option[_] => if (v.isDefined) 1 else 0
        case v: Traversable[_] => v.size
        case _ => throw new IllegalArgumentException(s"${v.getClass} is not supported")
      }

      override def test(v: A): Boolean = x.matches(size(v)).matches
    }
}

object ContainerMatchers extends ContainerMatchers
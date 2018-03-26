package cz.cvut.fit.prl.scalaimplicit.matcher

case class PropertyMatcher[A, B](name: String, property: A => B, matcher: Matcher[B]) extends Matcher[A] {
  override def description: String = name + " that " + matcher.description

  override def negativeDescription: String = name + " that " + matcher.negativeDescription

  override def describeMatch(v: A): Option[String] = matcher.describeMatch(property(v)).map(name + " " + _)

  override def describeMismatch(v: A): Option[String] = matcher.describeMismatch(property(v)).map(name + " " + _)

  override def test(v: A): Boolean = matcher.test(property(v))
}

case class BooleanPropertyMatcher[A](name: String, property: A => Boolean) extends Matcher[A] {
  override def description: String = s"is $name"

  override def negativeDescription: String = s"is not $name"

  override def describeMatch(v: A): Option[String] =
    if (test(v)) Some(s"is $name") else None

  override def describeMismatch(v: A): Option[String] =
    if (!test(v)) Some(s"$v is not $name") else None

  override def test(v: A): Boolean = property(v)
}

case class OptionPropertyMatcher[A, B](name: String, property: A => Option[B], matcher: Matcher[B]) extends Matcher[A] {
  override def test(v: A): Boolean = property(v).exists(matcher.test)

  override def description: String = name + " value that " + matcher.description

  override def negativeDescription: String = name + " value that " + matcher.negativeDescription

  override def describeMatch(v: A): Option[String] =
    property(v).flatMap(matcher.describeMatch).map(name + " value " + _)

  override def describeMismatch(v: A): Option[String] = property(v) match {
    case x@Some(_) => x.flatMap(matcher.describeMismatch).map(name + " value " + _)
    case None => Some(name + " is None")
  }
}

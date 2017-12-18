package cz.cvut.fit.prl.scalaimplicit.extraction

import cz.cvut.fit.prl.scalaimplicit.extractor.Location
import cz.cvut.fit.prl.scalaimplicit.extractor.Serializables.DeclaredImplicit
import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest

class DeclaredImplicitsTest extends SemanticdbTest {
  checkExtraction(
    "Basic information about declared implicits",
    """
      |package dI
      |object basicInfo {
      | implicit def m(a: String): String = "3"
      |}
    """.trim.stripMargin, { res =>
      res.normalizedImplicits should contain only DeclaredImplicit(
        location = Location.Empty,
        fqn = "_root_.dI.basicInfo.m(Ljava/lang/String;)Ljava/lang/String;.",
        signature =
          "(a: _root_.scala.Predef.String#): _root_.scala.Predef.String#",
        kind = "def",
        nargs = "1"
      )

      res.funs shouldBe empty
      res.links shouldBe empty
      res.params shouldBe empty
    }
  )

  checkExtraction(
    "Two declared implicits with identical code have different ids",
    """
      |package dI
      |object m1 {
      | implicit val msgDeclaration: String = "World"
      |}
      |object m2 {
      | implicit val msgDeclaration: String = "World"
      |}
    """.trim.stripMargin, { res =>
      res.normalizedImplicits should contain only (
        DeclaredImplicit(
          location = Location.Empty,
          fqn = "_root_.dI.m1.msgDeclaration.",
          signature = "_root_.scala.Predef.String#",
          kind = "val",
          nargs = "-1"
        ),
        DeclaredImplicit(
          location = Location.Empty,
          fqn = "_root_.dI.m2.msgDeclaration.",
          signature = "_root_.scala.Predef.String#",
          kind = "val",
          nargs = "-1"
        )
      )

      res.funs shouldBe empty
      res.links shouldBe empty
      res.params shouldBe empty

      res.implicits.groupBy(_.id).keys.toSet should have size 2
    }
  )

  checkExtraction(
    "The signature field indicates the type signature as expressed in the code",
    """
      |package dI
      |object typeInfo {
      | implicit def defType(a: String): String = "3"
      | implicit val valType = "3"
      |}
    """.trim.stripMargin, { res =>
      res.normalizedImplicits should contain only (
        DeclaredImplicit(
          location = Location.Empty,
          fqn =
            "_root_.dI.typeInfo.defType(Ljava/lang/String;)Ljava/lang/String;.",
          signature =
            "(a: _root_.scala.Predef.String#): _root_.scala.Predef.String#",
          kind = "def",
          nargs = "List()"
        ),
        DeclaredImplicit(
          location = Location.Empty,
          fqn = "_root_.dI.typeInfo.valType.",
          signature = "_root_.java.lang.String#",
          kind = "val",
          nargs = "-1"
        )
      )

      res.funs shouldBe empty
      res.links shouldBe empty
      res.params shouldBe empty

    }
  )

  checkExtraction(
    "Only the definition of an implicit is registered as a declared implicit",
    """
      |package dI
      |object defWithImplicits {
      |  def say(implicit a: String) = a
      |}
    """.trim.stripMargin, { res =>
      res.implicits should have size 1
    // As opposed to 2, for the reference to `a` in the body of `say`
    }
  )

  checkExtraction(
    "A declared implicit with a kind different to def has -1 nargs",
    """
      |package dI
      |object nonDefImplicitsNargs {
      | implicit val hello = 4
      | implicit object howdy {
      |   def say(implicit a: String) = a
      | }
      |}
    """.trim.stripMargin, { res =>
      res.implicits.map(_.nargs).toSet should contain only "-1"
    }
  )

  checkExtraction(
    "A declared implicit def has nargs equal to the number of non-implicit args",
    """
      |package dI
      |object defImplicitsNargs {
      |  implicit def say1(b: String, c: Int) = b
      |  implicit def say2(b: String, c: Int)(implicit a: String) = b
      |  implicit def say3(b: String)(c: Int) = b
      |}
    """.trim.stripMargin, { res =>
      res.implicits
        .filter(_.kind == "def")
        .map(_.nargs)
        .toSet should contain only "2"
    }
  )

  checkExtraction(
    "Type parameters are resolved to their fqn in the type signature",
    """
      |package iP
      |object defsWithTypeParams {
      | implicit def hello[A](m: A): String = "Hello"
      |}
    """.trim.stripMargin, { res =>
      res.implicits
        .map(_.signature) should contain only "[A] => (m: A): _root_.scala.Predef.String#"
    }
  )

  checkExtraction(
    "The signature in implicit objects should be empty",
    """
      |package iP
      |object implicitObjectSignature {
      | implicit object a {
      | }
      |}
    """.trim.stripMargin, { res =>
      res.implicits
        .map(_.signature) should contain only ""
    }
  )
}

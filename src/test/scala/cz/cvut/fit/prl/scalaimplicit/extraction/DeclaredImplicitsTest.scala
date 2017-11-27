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
        plainName = "m",
        fqtn = "_root_.scala.Predef.String#",
        signature = "(a: String): String",
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
          plainName = "msgDeclaration",
          fqtn = "_root_.scala.Predef.String#",
          signature = "String",
          kind = "val",
          nargs = "-1"
        ),
        DeclaredImplicit(
          location = Location.Empty,
          fqn = "_root_.dI.m2.msgDeclaration.",
          plainName = "msgDeclaration",
          fqtn = "_root_.scala.Predef.String#",
          signature = "String",
          kind = "val",
          nargs = "-1"
        )
      )

      res.funs shouldBe empty
      res.links shouldBe empty
      res.params shouldBe empty

      res.implicits.groupBy(_.id).keys.toSet.size shouldBe 2
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
          plainName = "defType",
          fqtn = "_root_.scala.Predef.String#",
          signature = "(a: String): String",
          kind = "def",
          nargs = "List()"
        ),
        DeclaredImplicit(
          location = Location.Empty,
          fqn = "_root_.dI.typeInfo.valType.",
          plainName = "valType",
          fqtn = "_root_.java.lang.String#",
          signature = "String",
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
    "The signature field of an implicit object is it's unqualified name",
    """
    package dI
    object objectTypeInfo {
     implicit object objType {
       val m = 4
     }
    }
    """.trim.stripMargin, { res =>
      val objImp = res.implicits.find(_.fqn.contains("objType")).get
      objImp.signature shouldBe "objType"
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
      res.implicits.size shouldBe 1 // As opposed to 2, for the reference to `a` in the body of `say`
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
      res.implicits.foreach(_.nargs == "-1")
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
      val definitions = res.implicits.filter(_.kind == "def")
      definitions.foreach(_.nargs shouldEqual "2")
    }
  )
}

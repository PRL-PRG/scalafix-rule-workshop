package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ExtractImplicits,
  ExtractionResult,
  Location,
  ReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.Serializables.DeclaredImplicit
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.Signature
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  Representation => r
}
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest

class DeclaredImplicitsTest extends SemanticdbTest {
  checkReflContext(
    "Basic information about declared implicits",
    """
      |package dI
      |object basicInfo {
      | implicit def m(a: String): String = "3"
      |}
    """.trim.stripMargin,
    ctx => {
      val res: ExtractionResult = ReflectExtract(ctx).normalized
      val expected = ExtractionResult(
        Seq(),
        Set(
          r.Declaration(
            name = "dI.basicInfo.m",
            kind = "def",
            location = r.Location(Some(r.Coordinates("", 2, 15)), false),
            isImplicit = true,
            signature = Some(
              Signature(
                typeParams = Seq(),
                parameterLists = Seq(
                  r.DeclaredParameterList(
                    params = Seq(
                      r.DeclaredParameter(
                        name = "a",
                        tipe = r.Type("java.lang.String")
                      )),
                    isImplicit = false
                  )
                ),
                returnType = Some(r.Type("String"))
              )),
            parents = Seq()
          )
        )
      )
      compareJSON(res, expected) shouldBe empty
    }
  )

  checkReflContext(
    "Two declared implicits with identical code have different ids",
    """
      |package dI
      |object m1 {
      | implicit val msgDeclaration: String = "World"
      |}
      |object m2 {
      | implicit val msgDeclaration: String = "World"
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ReflectExtract(ctx).normalized
      val resDecls = res.sortedDeclarations
      val expected = Seq(
        r.Declaration(
          name = "dI.m1.msgDeclaration",
          kind = "val",
          location = r.Location(Some(r.Coordinates("", 2, 28)), false),
          isImplicit = true,
          signature = Some(
            Signature(
              returnType = Some(r.Type("String"))
            )),
          parents = resDecls(0).parents
        ),
        r.Declaration(
          name = "dI.m2.msgDeclaration",
          kind = "val",
          location = r.Location(Some(r.Coordinates("", 5, 28)), false),
          isImplicit = true,
          signature = Some(
            Signature(
              returnType = Some(r.Type("String"))
            )),
          parents = resDecls(1).parents
        )
      )
      compareJSON(
        res,
        ExtractionResult(Seq(), expected.toSet)
      )
    }
  )
  /*
  checkReflContext(
    "The signature field indicates the type signature as expressed in the code",
    """
      |package dI
      |object typeInfo {
      | implicit def defType(a: String): String = "3"
      | implicit val valType = "3"
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ExtractImplicits(ctx)
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

  checkReflContext(
    "Only the definition of an implicit is registered as a declared implicit",
    """
      |package dI
      |object defWithImplicits {
      |  def say(implicit a: String) = a
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ExtractImplicits(ctx)
      res.implicits should have size 1
      // As opposed to 2, for the reference to `a` in the body of `say`
    }
  )

  checkReflContext(
    "A declared implicit with a kind different to def has -1 nargs",
    """
      |package dI
      |object nonDefImplicitsNargs {
      | implicit val hello = 4
      | implicit object howdy {
      |   def say(implicit a: String) = a
      | }
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ExtractImplicits(ctx)
      res.implicits.map(_.nargs).toSet should contain only "-1"
    }
  )

  checkReflContext(
    "A declared implicit def has nargs equal to the number of non-implicit args",
    """
      |package dI
      |object defImplicitsNargs {
      |  implicit def say1(b: String, c: Int) = b
      |  implicit def say2(b: String, c: Int)(implicit a: String) = b
      |  implicit def say3(b: String)(c: Int) = b
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ExtractImplicits(ctx)
      res.implicits
        .filter(_.kind == "def")
        .map(_.nargs)
        .toSet should contain only "2"
    }
  )

  checkReflContext(
    "Type parameters are resolved to their fqn in the type signature",
    """
      |package iP
      |object defsWithTypeParams {
      | implicit def hello[A](m: A): String = "Hello"
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ExtractImplicits(ctx)
      res.implicits
        .map(_.signature) should contain only "[A] => (m: A): _root_.scala.Predef.String#"
    }
  )

  checkReflContext(
    "The signature in implicit objects should be empty",
    """
      |package iP
      |object implicitObjectSignature {
      | implicit object a {
      | }
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ExtractImplicits(ctx)
      res.implicits
        .map(_.signature) should contain only ""
    }
  )
 */
}

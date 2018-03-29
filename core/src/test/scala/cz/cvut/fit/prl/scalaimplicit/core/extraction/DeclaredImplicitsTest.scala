package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ImplicitAnalysisResult,
  FailFastReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.{schema => s}
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
      val res: ImplicitAnalysisResult =
        FailFastReflectExtract(ctx).normalized.onlyImplicitDeclarations
      val expected = ImplicitAnalysisResult(
        Seq(),
        Set(
          s.Declaration(
            name = "dI.basicInfo.m",
            kind = "def",
            location = Some(s.Location("", 2, 40)),
            isImplicit = true,
            signature = Some(
              s.Signature(
                typeParameters = Seq(),
                parameterLists = Seq(
                  s.ParameterList(
                    parameters = Seq(
                      s.Parameter(
                        name = "a",
                        parameterType = s.Type("java.lang.String")
                      )),
                    isImplicit = false
                  )
                ),
                returnType = s.Type("String")
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
      val res = FailFastReflectExtract(ctx).normalized
      val resDecls = res.sortedDeclarations
      val expected = Seq(
        s.Declaration(
          name = "dI.m1.msgDeclaration",
          kind = "val",
          location = Some(s.Location("", 2, 28)),
          isImplicit = true,
          signature = Some(
            s.Signature(
              returnType = s.Type("String")
            )),
          parents = resDecls(0).parents
        ),
        s.Declaration(
          name = "dI.m2.msgDeclaration",
          kind = "val",
          location = Some(s.Location("", 5, 28)),
          isImplicit = true,
          signature = Some(
            s.Signature(
              returnType = s.Type("String")
            )),
          parents = resDecls(1).parents
        )
      )
      compareJSON(
        res,
        ImplicitAnalysisResult(Seq(), expected.toSet)
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
      res.parameters shouldBe empty

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
      |object defsWithtypeParameters {
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

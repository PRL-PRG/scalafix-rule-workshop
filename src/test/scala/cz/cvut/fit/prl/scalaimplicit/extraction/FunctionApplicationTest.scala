package cz.cvut.fit.prl.scalaimplicit.extraction

import cz.cvut.fit.prl.scalaimplicit.extractor.Location
import cz.cvut.fit.prl.scalaimplicit.extractor.Serializables.Apply
import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest

class FunctionApplicationTest extends SemanticdbTest {

  checkExtraction(
    "A function's nargs field does not count its implicit parameters",
    """
      |object implicitParameterCount {
      | implicit val msgDeclaration: String = "World"
      |
      | def message0Args()(implicit msgParamName: String): String = s"$msgParamName"
      | def message1Arg(greeting: String)(implicit msgParamName: String): String = s"$greeting $msgParamName"
      |
      | message0Args()
      | message1Arg("Hello")
      |}
    """.trim.stripMargin, { res =>
      res.normalizedFuns should contain only (
        Apply(
          location = Location.Empty,
          fqn =
            "_empty_.implicitParameterCount.message0Args(Ljava/lang/String;)Ljava/lang/String;.",
          code = "message0Args()",
          nargs = "0"
        ),
        Apply(
          location = Location.Empty,
          fqn =
            "_empty_.implicitParameterCount.message1Arg(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;.",
          code = "message1Arg(\"Hello\")",
          nargs = "1"
        )
      )
    }
  )

  checkExtraction(
    "If a function has implicit parameters but these are passed explicitly in the call site," +
      "it doesn't register as a function with implicits.",
    """
      |object explicitImplicitParameters {
      | implicit val msgDeclaration: String = "World"
      |
      | def say(greeting: String)(implicit msgParamName: String): String = s"$greeting $msgParamName"
      |
      | say("Hello")
      | say("Hello")("World")
      |}
    """.trim.stripMargin, { res =>
      res.funs should have size 1
    }
  )

  checkExtraction(
    "Two function call sites with identical code have different ids",
    """
        |object distinctIds {
        | implicit val msgDeclaration: String = "World"
        | def m()(implicit msg: String): String = s"Hello $msg"
        | m()
        | m()
        |}
      """.trim.stripMargin, { res =>
      res.normalizedFuns should contain only Apply(
        location = Location.Empty,
        fqn = "_empty_.distinctIds.m(Ljava/lang/String;)Ljava/lang/String;.",
        code = "m()",
        nargs = "0"
      )
      res.funs.map(_.id).distinct should have size 2
    }
  )

  checkExtraction(
    "All parameter lists are merged in the fqn",
    """
        |package a
        |object paramLists {
        | implicit val msgDeclaration: String = "World"
        | def say(greeting: String)(implicit msg: String): String = s"$msg"
        |
        | say("Hello")
        |}
      """.trim.stripMargin, { res =>
      res.funs should have size 1
      res.funs
        .map(_.fqn) should contain only "_root_.a.paramLists.say(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;."
    }
  )

}

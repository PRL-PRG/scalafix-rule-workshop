package cz.cvut.fit.prl.scalaimplicit.extraction

import cz.cvut.fit.prl.scalaimplicit.extractor.Serializables.Apply
import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest

class FunctionApplicationTest extends SemanticdbTest {

  checkExtraction(
    "A function's nargs field does not count its implicit parameters, " +
      "regardless of whether they are passed explicitly",
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
    res.funs.size shouldBe 2
    res.funs.find(_.code == "message0Args()").get.nargs shouldEqual "0"
    res.funs.find(_.code == "message1Arg(\"Hello\")").get.nargs shouldEqual "1"
  })

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
    res.funs.size shouldBe 1
  })

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
      res.funs.size shouldBe 2
      res.funs(0).code shouldEqual res.funs(1).code
      res.funs(0).fqn shouldEqual res.funs(1).fqn
      res.funs(0).id should not equal res.funs(1).id
  })
  
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
      res.funs.size shouldBe 1
      res.funs.head.fqn shouldEqual "_root_.a.paramLists.say(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;."
  })

}

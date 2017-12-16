package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.Serializables.ImplicitParam
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest

class ImplicitParameterTest extends SemanticdbTest {

  checkExtraction(
    "Basic example illustrating what each property means",
    """
      |package a
      |
      |object test {
      | implicit val msgDeclaration: String = "Hello"
      |
      | def message()(implicit msgParamName: String): String = msgParamName
      |
      | message()
      |}
    """.trim.stripMargin, { res =>
      res.params should contain only ImplicitParam(
        fqn = "_root_.a.test.msgDeclaration.",
        signature = "_root_.scala.Predef.String#",
        kind = "val"
      )
    }
  )

  checkExtraction(
    "Class and type names always coincide",
    """
      |case class M(msg: String)
      |
      |object classesAndTypes {
      | implicit val m = M("World")
      | def say(implicit word: M) = s"${word.msg}"
      | say
      |}
    """.trim.stripMargin, { res =>
      res.params should contain only ImplicitParam(
        fqn = "_empty_.classesAndTypes.m.",
        signature = "_empty_.M#",
        kind = "val"
      )
    }
  )

}

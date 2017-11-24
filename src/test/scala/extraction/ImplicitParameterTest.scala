package extraction

import extractor.Serializables.ImplicitParam
import framework.SemanticdbTest

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
      fqtn = "_root_.scala.Predef.String#",
      signature = "String",
      kind = "val",
      plainName = "msgDeclaration"
    )
  })
  
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
      fqtn =  "_empty_.M#",
      signature = "M",
      kind = "val",
      plainName = "m"
    )
  })
}

package extraction

import extractor.ImplicitParam
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
    res.params.size shouldBe 1
    val param: ImplicitParam = res.params.head
    param.id shouldEqual "_root_.a.test.msgDeclaration."
    param.clazz shouldEqual "_root_.scala.Predef.String#"
    param.typee shouldEqual "String"
    param.kind shouldEqual "val"
    param.name shouldEqual "msgDeclaration"
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
    res.params.size shouldBe 1
    val param: ImplicitParam = res.params.head
    param.id shouldEqual "_empty_.classesAndTypes.m."
    param.clazz shouldEqual "_empty_.M#"
    param.typee shouldEqual "M"
    param.kind shouldEqual "val"
    param.name shouldEqual "m"
  })
}

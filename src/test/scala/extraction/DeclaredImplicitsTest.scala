package extraction

import framework.SemanticdbTest

class DeclaredImplicitsTest extends SemanticdbTest {
  checkExtraction(
    "Basic information about declared implicits",
    """
      |package dI
      |object basicInfo {
      | implicit def m(a: String): String = "3"
      |}
    """.trim.stripMargin, { res =>
    res.implicits.size == 1
    val impl = res.implicits.head
    impl.fqn shouldEqual "_root_.dI.basicInfo.m(Ljava/lang/String;)Ljava/lang/String;."
    impl.clazz shouldEqual "_root_.scala.Predef.String#"
    impl.typee shouldEqual "(a: String): String"
    impl.kind shouldEqual "def"
  })

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
    res.implicits.size shouldBe 2
    val implicits = res.implicits.toSeq
    val m1 = implicits.find(_.fqn.contains("m1")).get
    val m2 = implicits.find(_.fqn.contains("m2")).get
    m1.path shouldEqual m2.path
    m1.fqn.replace("m1", "m2") shouldEqual m2.fqn
    m1.clazz shouldEqual m2.clazz
    m1.id should not equal m2.id
  })
  
  checkExtraction(
    "The typee field indicates the type signature as expressed in the code",
    """
      |package dI
      |object typeInfo {
      | implicit def defType(a: String): String = "3"
      | implicit val valType = "3"
      |}
    """.trim.stripMargin, { res =>
    res.implicits.size shouldBe 2
    val defImp = res.implicits.find(_.fqn.contains("defType")).get
    val valImp = res.implicits.find(_.fqn.contains("valType")).get
    defImp.typee shouldBe "(a: String): String"
    valImp.typee shouldBe "String"
  })
  
  checkExtraction(
    "The typee field of an implicit object is it's unqualified name",
    """
    package dI
    object objectTypeInfo {
     implicit object objType {
       val m = 4
     }
    }
    """.trim.stripMargin, { res =>
    val objImp = res.implicits.find(_.fqn.contains("objType")).get
    objImp.typee shouldBe "objType"
  })
}

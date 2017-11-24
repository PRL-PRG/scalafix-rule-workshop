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
    impl.fqtn shouldEqual "_root_.scala.Predef.String#"
    impl.signature shouldEqual "(a: String): String"
    impl.kind shouldEqual "def"
    impl.nargs shouldEqual "1"
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
    m1.location.path shouldEqual m2.location.path
    m1.fqn.replace("m1", "m2") shouldEqual m2.fqn
    m1.fqtn shouldEqual m2.fqtn
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
    defImp.signature shouldBe "(a: String): String"
    valImp.signature shouldBe "String"
  })
  
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
  })

  checkExtraction(
    "Only the definition of an implicit is registered as a declared implicit",
    """
      |package dI
      |object defWithImplicits {
      |  def say(implicit a: String) = a
      |}
    """.trim.stripMargin, { res =>
    res.implicits.size shouldBe 1 // As opposed to 2, for the reference to `a` in the body of `say`
  })

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
  })

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
  })
}

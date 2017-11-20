import extractor.{DeclaredImplicit, ImplicitParam}

class FQNTest extends SemanticdbTest {
  checkExtraction(
    "The fqn of something within a package starts with _root_ and ends with a dot",
    """
      |package a
      |object t {
      | implicit val hello: String = "Hello"
      |}
    """.trim.stripMargin, { res =>
    res.implicits.size shouldBe 1
    res.implicits.head.fqn should startWith("_root_")
    res.implicits.head.fqn should endWith(".")
  })

  checkExtraction(
    "The fqn of something in no package starts with _empty_ and ends with a dot",
    """
      |object globalT {
      | implicit val hello: String = "Hello"
      |}
    """.trim.stripMargin, { res =>
    res.implicits.size shouldBe 1
    res.implicits.head.fqn should startWith("_empty_")
    res.implicits.head.fqn should endWith(".")
  })


  checkExtraction(
    "Fq type names have a hashtag (#) at the end",
    """
      |object hashtags {
      | implicit val hello: String = "Hello"
      | def say()(implicit word: String) = word
      | say()
      |}
    """.trim.stripMargin, { res =>
    res.implicits.size shouldBe 3
    val declaredHello = res.implicits.find(_.fqn.contains("hello")).get
    declaredHello.fqn should startWith("_empty_")
    declaredHello.fqn  should endWith(".")
    declaredHello.fqn shouldEqual "_empty_.hashtags.hello."
    res.params.head.id shouldEqual "_empty_.hashtags.hello."
    res.params.head.clazz shouldEqual "_root_.scala.Predef.String#"
  })
  
  checkExtraction(
    "Lpath/to/class; notation is used as a type name in non-primitives in def signatures",
    """
      |package FQN
      |object Lnotation {
      | implicit def m(a: String): String = "3"
      |}
    """.trim.stripMargin, { res =>
      res.implicits.size shouldBe 1
      res.implicits.head.fqn shouldEqual "_root_.FQN.Lnotation.m(Ljava/lang/String;)Ljava/lang/String;."
  })

  checkExtraction(
    "The initial is used as a type name in primitives in def signatures (except Long and Boolean)",
    """
      |package FQN
      |object Inotation {
      | implicit def mI(a: Int): Int = a
      | implicit def mC(a: Char): Char = a
      | implicit def mL(a: Long): Long = a
      | implicit def mS(a: Short): Short = a
      | implicit def mF(a: Float): Float = a
      | implicit def mD(a: Double): Double = a
      | implicit def mBy(a: Byte): Byte = a
      | implicit def mBo(a: Boolean): Boolean = a
      |}
    """.trim.stripMargin, { res =>
      def find(n: String): DeclaredImplicit = {
        res.implicits.find(_.fqn.contains(n)).get
      }
      res.implicits.size shouldBe 8
      find("mI").fqn shouldEqual "_root_.FQN.Inotation.mI(I)I."
      find("mC").fqn shouldEqual "_root_.FQN.Inotation.mC(C)C."
      find("mL").fqn shouldEqual "_root_.FQN.Inotation.mL(J)J."
      find("mS").fqn shouldEqual "_root_.FQN.Inotation.mS(S)S."
      find("mF").fqn shouldEqual "_root_.FQN.Inotation.mF(F)F."
      find("mD").fqn shouldEqual "_root_.FQN.Inotation.mD(D)D."
      find("mBy").fqn shouldEqual "_root_.FQN.Inotation.mBy(B)B."
      find("mBo").fqn shouldEqual "_root_.FQN.Inotation.mBo(Z)Z."
   })
}

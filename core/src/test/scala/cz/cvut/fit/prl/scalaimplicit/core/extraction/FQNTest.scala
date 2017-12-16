package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.Location
import cz.cvut.fit.prl.scalaimplicit.core.extractor.Serializables._
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest

class FQNTest extends SemanticdbTest {
  checkExtraction(
    "The fqn of something within a package starts with _root_ and ends with a dot",
    """
      |package a
      |object t {
      | implicit val hello: String = "Hello"
      |}
    """.trim.stripMargin, { res =>
      res.implicits should have size 1
      res.implicits.head.fqn should startWith("_root_")
      res.implicits.head.fqn should endWith(".")
    }
  )

  checkExtraction(
    "The fqn of something in no package starts with _empty_ and ends with a dot",
    """
      |object globalT {
      | implicit val hello: String = "Hello"
      |}
    """.trim.stripMargin, { res =>
      res.implicits should have size 1
      res.implicits.head.fqn should startWith("_empty_")
      res.implicits.head.fqn should endWith(".")
    }
  )

  checkExtraction(
    "Fq type names have a hashtag (#) at the end",
    """
      |object hashtags {
      | implicit val hello: String = "Hello"
      | def say()(implicit word: String) = word
      | say()
      |}
    """.trim.stripMargin, { res =>
      res.implicits should have size 2
      res.normalizedImplicits should contain only (
        DeclaredImplicit(
          location = Location.Empty,
          fqn = "_empty_.hashtags.hello.",
          signature = "_root_.scala.Predef.String#",
          kind = "val",
          nargs = "-1"
        ),
        DeclaredImplicit(
          location = Location.Empty,
          fqn =
            "_empty_.hashtags.say(Ljava/lang/String;)Ljava/lang/String;.(word)",
          signature = "_root_.scala.Predef.String#",
          kind = "param",
          nargs = "-1"
        )
      )
      res.params should contain only ImplicitParam(
        fqn = "_empty_.hashtags.hello.",
        signature = "_root_.scala.Predef.String#",
        kind = "val"
      )
      res.normalizedFuns should contain only Apply(
        location = Location.Empty,
        fqn = "_empty_.hashtags.say(Ljava/lang/String;)Ljava/lang/String;.",
        code = "say()",
        nargs = "0"
      )
    }
  )

  checkExtraction(
    "Lpath/to/class; notation is used as a type name in non-primitives in def signatures",
    """
      |package FQN
      |object Lnotation {
      | implicit def m(a: String): String = "3"
      |}
    """.trim.stripMargin, { res =>
      res.implicits
        .map(_.fqn) should contain only "_root_.FQN.Lnotation.m(Ljava/lang/String;)Ljava/lang/String;."
    }
  )

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
      res.implicits.map(_.fqn) should contain only (
        "_root_.FQN.Inotation.mI(I)I.",
        "_root_.FQN.Inotation.mC(C)C.",
        "_root_.FQN.Inotation.mL(J)J.",
        "_root_.FQN.Inotation.mS(S)S.",
        "_root_.FQN.Inotation.mF(F)F.",
        "_root_.FQN.Inotation.mD(D)D.",
        "_root_.FQN.Inotation.mBy(B)B.",
        "_root_.FQN.Inotation.mBo(Z)Z."
      )
    }
  )
}

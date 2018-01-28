package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  FailFastReflectExtract,
  ReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.ImplicitArgument
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest

class KindTests extends SemanticdbTest {
  checkReflContext(
    "ArrowAssoc should have kind class",
    """
      |package kinds
      |object arrow {
      | "a" -> 4
      |}
    """.trim.stripMargin,
    ctx => {
      val css = FailFastReflectExtract(ctx)
      css.sortedCallSites should have size 1
      css.sortedCallSites.head.name.contains("ArrowAssoc") shouldBe true
      css.sortedCallSites.head.declaration.kind.contains("class") shouldBe true
    }
  )

  checkReflContext(
    "Calling implicit classes",
    """
      |package kinds
      |object iclass {
      | case class A(i: Int)
      | implicit class N(i: A) { def s() = ??? }
      | A(3).s
      |}
    """.trim.stripMargin,
    ctx => {
      val css = FailFastReflectExtract(ctx)
      css.sortedCallSites should have size 1
      css.sortedCallSites.head.name.contains("N") shouldBe true
      css.sortedCallSites.head.declaration.kind.contains("class") shouldBe true
    }
  )

  checkReflContext(
    "Calling implicit defs",
    """
      |package kinds
      |object idefs {
      | case class A()
      | implicit def stoi(s: A): Int = 4
      | A() + 4
      |}
    """.trim.stripMargin,
    ctx => {
      val css = FailFastReflectExtract(ctx)
      css.sortedCallSites should have size 1
      css.sortedCallSites.head.name.contains("stoi") shouldBe true
      css.sortedCallSites.head.declaration.kind.contains("def") shouldBe true
    }
  )

  checkReflContext(
    "Implicit vals as parameters",
    """
      |package kinds
      |object valparam {
      | implicit val a: String = "a"
      | def f(i: Int)(implicit a: String) = ???
      | f(4)
      |}
    """.trim.stripMargin,
    ctx => {
      val css = FailFastReflectExtract(ctx)
      css.sortedCallSites should have size 1
      val cs = css.sortedCallSites.head
      cs.name.contains("f") shouldBe true
      cs.declaration.kind.contains("def") shouldBe true
      cs.implicitArguments.size shouldBe 1
      val arg = cs.implicitArguments.head.asInstanceOf[ImplicitArgument]
      arg.declaration.kind.contains("val") shouldBe true
    }
  )

  checkReflContext(
    "Implicit objects as parameters",
    """
      |package kinds
      |object objparam {
      | class A()
      | implicit object B extends A { }
      | def f[T](i: Int)(implicit a: A) = ???
      | f(4)
      |}
    """.trim.stripMargin,
    ctx => {
      val css = FailFastReflectExtract(ctx)
      css.sortedCallSites should have size 1
      val cs = css.sortedCallSites.head
      cs.name.contains("f") shouldBe true
      cs.declaration.kind.contains("def") shouldBe true
      cs.implicitArguments.size shouldBe 1
      val arg = cs.implicitArguments.head.asInstanceOf[ImplicitArgument]
      arg.declaration.kind.contains("object") shouldBe true
    }
  )

  checkReflContext(
    "Implicit defs as parameters",
    """
      |package kinds
      |object defparam {
      | class A()
      | implicit def b: A = ???
      | def f[T](i: Int)(implicit a: A) = ???
      | f(4)
      |}
    """.trim.stripMargin,
    ctx => {
      val css = FailFastReflectExtract(ctx)
      css.sortedCallSites should have size 1
      val cs = css.sortedCallSites.head
      cs.name.contains("f") shouldBe true
      cs.declaration.kind.contains("def") shouldBe true
      cs.implicitArguments.size shouldBe 1
      val arg = cs.implicitArguments.head.asInstanceOf[ImplicitArgument]
      arg.declaration.kind.contains("def") shouldBe true
    }
  )
}

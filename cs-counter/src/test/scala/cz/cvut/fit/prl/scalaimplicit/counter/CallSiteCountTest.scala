package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.FailFastReflectExtract
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest

class CallSiteCountTest extends SemanticdbTest {

  checkReflContext(
    "Class instantiations are counted as call sites",
    """
      |package csCount
      |object news {
      | case class A()
      | val a = new A()
      | val b = A()
      |}
    """.trim.stripMargin, ctx => {
      FailFastReflectExtract(ctx).totalCallSites shouldBe 2
    }
  )

  checkReflContext(
    "Anonymous classes are counted as call sites",
    """
      |package csCount
      |object anon {
      | trait A {}
      | val a = new A {}
      | val b = new A {}
      |}
    """.trim.stripMargin, ctx => {
      FailFastReflectExtract(ctx).totalCallSites shouldBe 2
    }
  )

  checkReflContext(
    "Methods without parameters are counted as call sites",
    """
      |package csCount
      |object noparam {
      | def foo: String = ???
      | val a = foo
      | val b = foo
      |}
    """.trim.stripMargin, ctx => {
      FailFastReflectExtract(ctx).totalCallSites shouldBe 3
    }
  )


  checkReflContext(
    "Implicit conversions are counted",
    """
      |package csCount
      |object conversion {
      | trait A { def foo() }
      | implicit def int2A(i: Int): A = ???
      | 3.foo
      |}
    """.trim.stripMargin, ctx => {
      FailFastReflectExtract(ctx).totalCallSites shouldBe 3
    }
  )
}

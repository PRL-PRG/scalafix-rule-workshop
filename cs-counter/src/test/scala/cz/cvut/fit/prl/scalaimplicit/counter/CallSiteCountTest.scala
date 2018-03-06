package cz.cvut.fit.prl.scalaimplicit.counter

class CallSiteCountTest extends CallSiteCountTestSuite {

  checkDB(
    "Class instantiations are counted",
    """
      |package csCount
      |object news {
      | case class A()
      | val a = new A() // Counted
      | val b = A.apply() // Counted
      | val c = A() // Counted as synthetic *.apply
      |}
    """.trim.stripMargin,
    db => {
      CountCallSites
        .processDB(db)
        .normalized should contain only CallSiteCount("", 2, 1)
    }
  )

  checkDB(
    "Anonymous classes are counted as call sites",
    """
      |package csCount
      |object anon {
      | trait A {}
      | val a = new A {}
      | val b = new A {}
      |}
    """.trim.stripMargin,
    db => {
      CountCallSites
        .processDB(db)
        .normalized should contain only CallSiteCount("", 2, 0)
    }
  )

  checkDB(
    "Methods without parameters are counted as call sites",
    """
      |package csCount
      |object noparam {
      | def foo: String = ???
      | val a = foo
      | val b = foo
      |}
    """.trim.stripMargin,
    db => {
      CountCallSites
        .processDB(db)
        .normalized should contain only CallSiteCount("", 3, 0)
    }
  )

  checkDB(
    "Implicit conversions are counted",
    """
      |package csCount
      |object conversion {
      | trait A { def foo() }
      | implicit def int2A(i: Int): A = ???
      | 3.foo
      |}
    """.trim.stripMargin,
    db => {
      CountCallSites
        .processDB(db)
        .normalized should contain only CallSiteCount("", 2, 1)
    }
  )
}

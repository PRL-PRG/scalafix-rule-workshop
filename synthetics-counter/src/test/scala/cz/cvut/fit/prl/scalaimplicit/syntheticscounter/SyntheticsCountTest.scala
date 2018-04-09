package cz.cvut.fit.prl.scalaimplicit.syntheticscounter

class SyntheticsCountTest extends SyntheticCountsTestSuite {

  checkDB(
    "Applies are counted",
    """
      |package synthCount
      |object applies {
      | case class A()
      | val c = A() // Counted as synthetic *.apply
      | val a = A.apply() // Not counted
      |}
    """.trim.stripMargin,
    db => {
      CountSynthetics
        .processDB(db).normalized should contain only SyntheticCount("", 1)
    }
  )


  checkDB(
    "Implicit parameters are counted",
    """
      |package synthCount
      |object params {
      | def foo()(implicit a: String) = ???
      | implicit val b: String = "Hello"
      | foo() // Counted
      | foo() // Counted
      | foo()(b) // Not Counted
      |}
    """.trim.stripMargin,
    db => {
      CountSynthetics
        .processDB(db).normalized should contain only SyntheticCount("", 2)
    }
  )


  checkDB(
    "Implicit conversions are counted",
    """
      |package synthCount
      |object conversions {
      | implicit def foo(a: Int): String = ???
      | implicit def bar(a: String)(implicit p: String): Int = ???
      | implicit val hello: String = "Hello"
      |
      | val a: Int = "Hi"
      | val b: String = 42
      |}
    """.trim.stripMargin,
    db => {
      CountSynthetics
        .processDB(db).normalized should contain only SyntheticCount("", 2)
    }
  )

  checkDB(
    "Type arguments are counted",
    """
      |package synthCount
      |object targs {
      | implicit def foo[A](a: A): String = ???
      |
      | val a: String = foo(23)
      | val b: String = 23
      | val c: String = foo[Int](23)
      |}
    """.trim.stripMargin,
    db => {
      CountSynthetics
        .processDB(db).normalized should contain only SyntheticCount("", 2)
    }
  )

  checkDB(
    "All in one",
    """
      |package synthCount
      |object all {
      | case class O()
      | object O {
      |   implicit def apply[A](a: A): O = ???
      | }
      |
      | val a: O = 23
      | val b: O = "HI"
      |}
    """.trim.stripMargin,
    db => {
      CountSynthetics
        .processDB(db).normalized should contain only SyntheticCount("", 2)
    }
  )
}

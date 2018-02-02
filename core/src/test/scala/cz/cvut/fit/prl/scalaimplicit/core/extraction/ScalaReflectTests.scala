package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest

class ScalaReflectTests extends SemanticdbTest {
  checkReflMirror(
    "Implicit classes are inner classes, cannot be loaded with dots from classes",
    """
      |package refltest
      |package implicitsAreInners
      |class A1 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |abstract class A2 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |trait A3 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |case class A4() {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
    """.stripMargin,
    mirror => {
      def checkDirectLoadFails(what: String): Unit = {
        a[Throwable] should be thrownBy mirror.staticClass(what)
        a[Throwable] should be thrownBy mirror.staticModule(what)
        a[Throwable] should be thrownBy mirror.staticPackage(what)
      }
      // A1: Plain class
      checkDirectLoadFails("refltest.implicitsAreInners.A1.B")
      // A2: Abstract class
      checkDirectLoadFails("refltest.implicitsAreInners.A2.B")
      // A3: Trait
      checkDirectLoadFails("refltest.implicitsAreInners.A3.B")
      // A4: Case class
      checkDirectLoadFails("refltest.implicitsAreInners.A4.B")
    }
  )

  checkReflMirror(
    "Implicit classes inside object CAN be loaded with dots",
    """
      |package refltest
      |package implicitsAreInners
      |object A5 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
    """.stripMargin,
    mirror => {
      val B = mirror.staticClass("refltest.implicitsAreInners.A5.B")
      B.isImplicit shouldBe (true)
    }
  )

  checkReflMirror(
    "To load the proper scala classes, we need to look into the outer classes",
    """
      |package refltest
      |package loadInnerClasses
      |class A1 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |abstract class A2 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |trait A3 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |case class A4() {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
    """.stripMargin,
    mirror => {
      def classLoad(owner: String, name: String) =
        mirror
          .staticClass(owner)
          .typeSignature
          .decls
          .sorted
          .find(_.name.toString == name)
          .get
          .isImplicit shouldBe (true)

      classLoad("refltest.implicitsAreInners.A1", "B")
      classLoad("refltest.implicitsAreInners.A2", "B")
      classLoad("refltest.implicitsAreInners.A3", "B")
      classLoad("refltest.implicitsAreInners.A4", "B")
    }
  )

  checkReflMirror(
    "Inner classes can be loaded with dollar sign notation, but they are the JAVA classes",
    """
      |package refltest
      |package innerDollars
      |class A1 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |abstract class A2 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |trait A3 {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
      |case class A4() {
      | implicit class B(i: Int) {
      |     def b: String = ???
      |  }
      |}
    """.stripMargin,
    mirror => {
      mirror
        .staticClass("refltest.innerDollars.A1$B")
        .isImplicit shouldBe (false)
      mirror
        .staticClass("refltest.innerDollars.A2$B")
        .isImplicit shouldBe (false)
      mirror
        .staticClass("refltest.innerDollars.A3$B")
        .isImplicit shouldBe (false)
      mirror
        .staticClass("refltest.innerDollars.A4$B")
        .isImplicit shouldBe (false)
    }
  )

  checkReflMirror(
    "Vals inside Vals are represented as ...",
    """
      |package refltest
      |package valception
      |case class A() {
      | private val outerval: Map[Int, String] = {
      |    var innerval = 3
      |    Map(innerval -> "3")
      | }
      |}
    """.stripMargin,
    mirror => {
      // TODO: Currently we have no idea how to access innerval with reflection
      println(mirror)
    }
  )
}

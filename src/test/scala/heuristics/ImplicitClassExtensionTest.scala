package heuristics

import framework.SemanticdbTest

class ImplicitClassExtensionTest extends SemanticdbTest {

  checkContext(
    "From the repo wiki: https://github.com/PRL-PRG/scalafix-rule-workshop/wiki/Patterns:-Implicit-Class-Extension",
    """
      |package a
      |object Definition {
      | implicit class Hello(s: String) { def hello = s"Hello, $s" }
      |}
      |object Usage {
      | import Definition._
      | println("World".hello)
      |}
    """.trim.stripMargin, { ctx =>
    println(1)
  })

  checkContext(
    "From the official documentation: https://docs.scala-lang.org/overviews/core/implicit-classes.html",
    """
      |package b
      |// Definition
      |object Definition {
      |  implicit class IntWithTimes(x: Int) {
      |    def times[A](f: => A): Unit = {
      |      def loop(current: Int): Unit =
      |        if(current > 0) {
      |          f
      |          loop(current - 1)
      |        }
      |      loop(x)
      |    }
      |  }
      |}
      |object Usage {
      | import Definition._
      | 5 times println("HI")
      |}
      |
    """.trim.stripMargin, { ctx =>
      // TODO: Custom assertions on the context
  })

  checkContext(
    "From aerosolve: com/airbnb/aerosolve/training/pipeline/PipelineTestingUtil.scala",
    """
      |package c
      |object Definition {
      | implicit class Tupple2Add(t: (Long, Long)) {
      |    def +(p: (Long, Long)) = (p._1 + t._1, p._2 + t._2)
      |  }
      |}
      |object Usage {
      | import Definition._
      | (2L, 4L) + (3L, 6L)
      |}
    """.trim.stripMargin, { ctx =>
      // TODO: Custom assertions on the context
  })
}

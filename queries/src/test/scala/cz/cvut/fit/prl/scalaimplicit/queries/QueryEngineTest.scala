package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ReflectExtract
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import cz.cvut.fit.prl.scalaimplicit.macros.QueryEngineMacros._

class QueryEngineTest extends SemanticdbTest {
  checkReflContext(
    "Class Conversion",
    """
      |object classConv {
      |trait UselessParent {}
      |trait Useless extends UselessParent {}
      | trait Writer[A] {
      |  def write(x: A): String
      | }
      | implicit object IntWriter extends Writer[Int] with Useless {
      |  def write(x: Int) = (x * 2).toString
      | }
      | implicit class Hello[T: Writer](s: T) { def hello(): String = implicitly[Writer[T]].write(s) }
      | println( 2.hello() )
      |}
    """.trim.stripMargin, { ctx =>
      import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite
      val res = ReflectExtract(ctx)
      val query = qcsm(("name", "scala.Predef.implicitly"))
      val qres = QueryEngine(query, res)
      println(qres)
      println("End")
    }
  )
}

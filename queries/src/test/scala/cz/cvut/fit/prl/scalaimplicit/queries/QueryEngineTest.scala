package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ReflectExtract
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.Declaration
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.JSONSerializer
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

      val q1 = qcs(
        """CallSite("scala.Predef.implicitly",_,_,_,Declaration("scala.Predef.implicitly",_,_,_,_,_),_,_)""",
        "")
      val qres = QueryEngine(q1, res)
      println(qres)
      println("--------------------------------------")
      val q2 =
        qcs("""CallSite(ss,_,_,_,Declaration(xx,_,_,_,_,_),_,_)""", "ss == xx")
      val qres2 = QueryEngine(q2, res)
      println(qres2)
      println("--------------------------------------")
      // All the non-synthetic calls to implicit defs (matches only implicitly[]())
      val q3 =
        qcs("""CallSite(_,_,_,false,Declaration(_,_,_,true,_,_),_,_)""", "")
      val qres3 = QueryEngine(q2, res)
      println(qres3)
      println("--------------------------------------")
      println("End")
    }
  )

  checkReflContext(
    "Serialization test",
    """
      |object serialization {
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
      val res = ReflectExtract(ctx)

      JSONSerializer.save(res, "./res.dat")
      val loaded = JSONSerializer.load("./res.dat")
      println(loaded)
    }
  )
}

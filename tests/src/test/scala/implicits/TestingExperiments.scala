package implicits

import fix.BuildInfo
import org.scalatest._

import scala.meta.{AbsolutePath, Classpath}
import scala.meta._
import scalafix.SemanticdbIndex

class TestingExperiments extends FunSuite {
  val index = SemanticdbIndex.load(Classpath(AbsolutePath(BuildInfo.inputClassdirectory)))


  test("Random code example") {
    /* Not found an easy way to just generate semanticdb indices
        from strings, since they have to be compiled first.
        The closest I think we can get is with Database.load
     */
    val unit = """
                 class A {
                   import ExecutionContext.Implicit.global
                   Future(1)
                 }
               """

    val res = ImplicitContextCSV(index)

    assert(res.funs.size == 1)
  }
}

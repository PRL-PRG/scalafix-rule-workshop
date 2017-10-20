package implicits

import fix.BuildInfo
import org.scalatest._

import scala.meta.{AbsolutePath, Classpath}
import scala.meta._
import scalafix.{RuleCtx, SemanticdbIndex}

class TestingExperiments extends FunSuite {
  val index = SemanticdbIndex.load(Classpath(AbsolutePath(BuildInfo.inputClassdirectory)))


  test("Random code example") {
    /* TODO:
        Not found an easy way to just generate semanticdb indices
        from strings, since they have to be compiled first.
        The closest I think we can get is with Database.load

        Quasiquotes give us the tree, but not the semanticdb
     */
    val unit = """
                 class A {
                   import ExecutionContext.Implicit.global
                   Future(1)
                 }
               """

    // TODO:
    // We'll need to create the auxiliary function `run` to
    // return the tuples/objects with the desired values.
    //
    // Not really hard, just ran out of time

    val res = ImplicitContextCSV(index)


    //assert(res.funs.size == 1)
  }
}

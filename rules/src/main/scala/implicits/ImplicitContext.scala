package implicits

import scalafix._
import scala.meta._
import scala.meta.contrib._

final case class ImplicitContext(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitContext")  {

 
  override def fix(ctx: RuleCtx): Patch = {
    //ctx.debugIndex()
    ctx.tree.collect {
      case x: Term.Apply => {
        println(s"Found Regular function call ${x.structure}")
      }
    }
    ctx.index.database.synthetics.collect {
      case x : Synthetic => {
        val syntax = x.names(1).syntax.split(" ")
        val name = syntax(1)
        val callTemplate = ".*${name}(.*)".r
        val isCall : Boolean = syntax(3) match {
          case callTemplate() => {
            println(s"Found Synthetic Method Call ${syntax(3)}")
            true
          }
          case x => {
            println(x)
            false
          }
        }

      }
    }
    Patch.empty
  }
}

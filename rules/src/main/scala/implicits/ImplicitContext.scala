package implicits

import scala.meta._
import scala.meta.contrib._
import scalafix.lint.{LintCategory, LintSeverity}
import scalafix.syntax._
import scalafix.rule.RuleCtx
import scalafix.{LintMessage, Patch, SemanticRule, SemanticdbIndex}


final case class ImplicitContext(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitContext")  {

 
  override def fix(ctx: RuleCtx): Patch = {
    //ctx.debugIndex()

    val explicitSymbols = ctx.tree.collect {
      case node: Defn.Object if node.hasMod(mod"implicit") => node
    }
    println(explicitSymbols)

    var syntheticSymbols : List[Symbol] = List[Symbol]()
    for {synth <- ctx.index.database.synthetics} {
      synth.names(1).symbol.denotation match {
        case Some(den) => if (den.isImplicit) {
          syntheticSymbols = syntheticSymbols ++ List(synth.names(1).symbol)
        }
        case None => {}
      }
    }
    println(syntheticSymbols)

    Patch.empty
  }
}

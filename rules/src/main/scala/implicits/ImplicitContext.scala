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
    ctx.debugIndex()
    ctx.tree.collect {
      case x: Term.Apply => {
        println(s"Found Regular function call ${x.structure}")
      }
    }
    for {synth <- ctx.index.database.synthetics} {
      println(synth.names(1).symbol.denotation)
    }
    Patch.empty
  }
}

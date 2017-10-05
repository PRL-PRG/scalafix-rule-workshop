package implicits

import scala.meta._
import scala.meta.contrib._
import scalafix.syntax._
import scalafix.rule.RuleCtx
import scalafix.{Patch, SemanticRule, SemanticdbIndex}

case class ImplicitTypeClass(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitTypeClass") {

  override def fix(ctx: RuleCtx): Patch = {
    ctx.tree.collect {
      case x: Defn.Object if x.hasMod(mod"implicit") => {
        val symbol = x.name.symbol

        println(s"Implicit object ${x.name} -- $symbol")
      }
    }

    Patch.empty
  }
}
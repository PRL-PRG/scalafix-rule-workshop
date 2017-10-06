package implicits

import scala.meta._
import scala.meta.contrib._
import scalafix.lint.{LintCategory, LintSeverity}
import scalafix.syntax._
import scalafix.rule.RuleCtx
import scalafix.{LintMessage, Patch, SemanticRule, SemanticdbIndex}

case class ImplicitTypeClass(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitTypeClass") {

  val implicitClass =
    LintCategory("ImplicitTypeClass", "", LintSeverity.Info)

  override def check(ctx: RuleCtx): Seq[LintMessage] = ctx.tree.collect {
    case x: Defn.Object if x.hasMod(mod"implicit") => {
      implicitClass.at(s"${x.name.value} is an implicit class", x.pos)
    }
  }

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
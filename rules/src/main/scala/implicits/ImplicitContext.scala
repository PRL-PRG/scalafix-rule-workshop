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

    var syntheticImplicits : Map[Int, Symbol] = Map[Int, Symbol]()
    for {synth <- ctx.index.database.synthetics} {
      synth.names(1).symbol.denotation match {
        case Some(den) => if (den.isImplicit) {
          syntheticImplicits = syntheticImplicits ++ Map(synth.position.start -> synth.names(1).symbol)
        }
        case None => {}
      }
    }
    println(s"Synthetic Implicits: ${syntheticImplicits}")

    var explicitSymbols : List[Tree] = List[Tree]()
    for {node <- ctx.tree} {
      node match {
        case node: Defn.Object => {
          if (node.hasMod(mod"implicit")) {
            explicitSymbols = explicitSymbols ++ List(node)
          }
        }
        case node: Defn.Val => {
          if (node.hasMod(mod"implicit")) {
            explicitSymbols = explicitSymbols ++ List(node)
          }
        }
        case node: Defn.Def => {
          if (node.hasMod(mod"implicit")) {
            explicitSymbols = explicitSymbols ++ List(node)
          }
        }
        case node: Term.Param =>

        /*{node.symbol match{
          case Some(symbol) => symbol.denotation match {
            case Some(denotation) => {
              if (denotation.isImplicit) {
                explicitSymbols = explicitSymbols ++ List(node)
              }
            }
            case None => {}
          }
          case None => {}
        }}*/
        case node: Term.Apply => {
          // For some reason it doesn't recognize the denotation
          val end = node.pos.end
          val implicitParams = syntheticImplicits.collect {
            case (end, sym) => sym
          }
          println(implicitParams)
        }
        case _ => {}
      }
    }
    println(explicitSymbols)

    Patch.empty
  }
}

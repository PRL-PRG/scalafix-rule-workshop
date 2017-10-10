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

    println(s"Symbol collection -------------------------")

    val syntheticImplicits: List[(Int, Symbol)] = collectSyntheticImplicits(ctx)
    println(s"Synthetic Implicits: ${syntheticImplicits}")
    val (treeImplicits, callsWithImplicitParameters) = processExplicitTree(ctx, syntheticImplicits)
    println(s"Explicit Symbols: ${treeImplicits}")
    println(s"Calls With Implicit Parameters: ${callsWithImplicitParameters}")

    println(s"Analysis -------------------------")
    processCalls(treeImplicits, syntheticImplicits, callsWithImplicitParameters)

    Patch.empty
  }

  def processCalls(treeImplicits: List[Tree], syntheticImplicits: List[(Int, Symbol)], callsWithImplicitParameters: Map[Tree, List[Symbol]]) : Unit = {
    for {call <- callsWithImplicitParameters} {
      println(s"Call with implicit parameters:")
      println(s"  Called function: ${call._1.syntax}")
      println(s"  Implicit parameters:")
      for {param <- call._2} {
        println(s"    ${param.syntax}")
      }
      println(s"")
    }
  }

  def collectSyntheticImplicits(ctx: RuleCtx) = {
    var syntheticImplicits: List[(Int, Symbol)] = List[(Int, Symbol)]()
    for {synth <- ctx.index.database.synthetics} {
      for {name <- synth.names} {
        name.symbol.denotation match {
          case Some(denotation) => if (denotation.isImplicit) {
            syntheticImplicits = syntheticImplicits ++ List((synth.position.start, name.symbol))
          }
          case None => {}
        }
      }
    }
    syntheticImplicits
  }

  def processExplicitTree(ctx: RuleCtx, syntheticImplicits:  List[(Int, Symbol)]) : (List[Tree], Map[Tree, List[Symbol]]) = {
    var explicitSymbols : List[Tree] = List[Tree]()
    var callsWithImplicitParameters : Map[Tree, List[Symbol]] = Map[Tree, List[Symbol]]()
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
          val end = node.pos.end
          val implicitParams : List[Symbol] = syntheticImplicits.collect{
            case (pos, sym) if pos == end => sym
          }(collection.breakOut)
          println(s"Implicit parameters when calling ${node.fun.syntax}: ${implicitParams}")

          if (implicitParams.nonEmpty) {
            callsWithImplicitParameters = callsWithImplicitParameters ++ Map(node -> implicitParams)
          }
        }
        case _ => {}
      }
    }
    (explicitSymbols, callsWithImplicitParameters)
  }
}

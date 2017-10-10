package implicits

import scala.meta._
import scala.meta.contrib._
import scalafix.lint.{LintCategory, LintSeverity}
import scalafix.syntax._
import scalafix.rule.RuleCtx
import scalafix.{LintMessage, Patch, SemanticRule, SemanticdbIndex}

case class SyntheticImplicitParameters(syntheticSymbol: Synthetic, objectDeclaration: Option[Tree])

final case class ImplicitContext(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitContext")  {

  override def fix(ctx: RuleCtx): Patch = {
    //ctx.debugIndex()

    println(s"Symbol collection -------------------------")

    val syntheticImplicits: List[(Int, Synthetic)] = collectSyntheticImplicits(ctx)
    println(s"Synthetic Implicits: ${syntheticImplicits}")
    val (treeImplicits, callsWithImplicitParameters) = processExplicitTree(ctx, syntheticImplicits)
    println(s"Explicit Symbols: ${treeImplicits}")
    println(s"Calls With Implicit Parameters: ${callsWithImplicitParameters}")

    // TODO: Debug 0
    for {x <- callsWithImplicitParameters} x._2.names(2).symbol.denotation.get


    println(s"Analysis -------------------------")
    Format.reportCalls(index, ctx, treeImplicits, syntheticImplicits, callsWithImplicitParameters)

    Patch.empty
  }

  def collectSyntheticImplicits(ctx: RuleCtx) = {
    var syntheticImplicits: List[(Int, Synthetic)] = List[(Int, Synthetic)]()
    for {synth <- ctx.index.database.synthetics} {
      for {name <- synth.names} {
        name.symbol.denotation match {
          case Some(denotation) => if (denotation.isImplicit) {
            syntheticImplicits = syntheticImplicits ++ List((synth.position.start, synth))
          }
          case None => {}
        }
      }
    }
    syntheticImplicits
  }

  def processExplicitTree(ctx: RuleCtx, syntheticImplicits:  List[(Int, Synthetic)]) : (List[Tree], Map[Term.Apply, Synthetic]) = {
    var explicitSymbols : List[Tree] = List[Tree]()
    var callsWithImplicitParameters : Map[Term.Apply, Synthetic] = Map[Term.Apply, Synthetic]()
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
        case node: Term.Apply => {
          val end = node.pos.end
          syntheticImplicits.find { _._1 ==  end} match {
            case Some(synthetic) => {

              // TODO: Debug 1
              for {x <- callsWithImplicitParameters} {
                x._2.names(2).symbol.denotation.get
              }

              callsWithImplicitParameters = callsWithImplicitParameters ++ Map(node -> synthetic._2)
            }
            case None => None
          }
        }
        case _ => {}
      }
    }
    (explicitSymbols, callsWithImplicitParameters)
  }
}


object Format {

  def reportCalls(index: SemanticdbIndex, ctx: RuleCtx, treeImplicits: List[Tree], syntheticImplicits: List[(Int, Synthetic)], callsWithImplicitParameters: Map[Term.Apply, Synthetic]) : Unit = {
    // TODO: Debug 2. Why can't I get the denotation here, but I can get it in the other two Debugs?
    for {x <- callsWithImplicitParameters} x._2.names(2).symbol.denotation.get
    for {call <- callsWithImplicitParameters} {
      val function = call._1
      println(s"Call with implicit parameters:")
      println(s"  Called function: ${formatLocation(function)}: ${call._1.syntax}")
      println(s"  Declaration: ${}")
      println(s"  Implicit parameters:")
      for {param <- call._2.names} {
        println(s"    ${param.syntax}${param.symbol}")
        //println(s"    ${param.syntax}${param.symbol.denotation match {case Some(denot) => s", declared in ${formatLocation(param)}: $denot" case None => ""}}")
      }
      println(s"")
    }
  }

  def formatLocation(tree: Tree) : String = {
    s"${getFileName(tree.input)}@[l:${tree.pos.startLine}, c:${tree.pos.endLine}]"
  }

  def getFileName(in: Input) : String = {
    in match {
      case virtualFile: Input.VirtualFile => virtualFile.path
      case regularFile: Input.File => regularFile.path.toString()
      case _ => s"Input type Unknown.\n Here is the full Input: \n $in"
    }
  }
}

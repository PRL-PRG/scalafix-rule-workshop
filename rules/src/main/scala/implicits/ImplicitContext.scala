package implicits

import scala.meta._
import scala.meta.contrib._
import scalafix.lint.{LintCategory, LintSeverity}
import scalafix.syntax._
import scalafix.rule.RuleCtx
import scalafix.{LintMessage, Patch, SemanticRule, SemanticdbIndex}

case class SyntheticImplicitParameter(syntheticSymbol: Symbol, objectDeclaration: Option[Tree])

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
    Format.reportCalls(ctx, treeImplicits, syntheticImplicits, callsWithImplicitParameters)

    Patch.empty
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

  def processExplicitTree(ctx: RuleCtx, syntheticImplicits:  List[(Int, Symbol)]) : (List[Tree], Map[Term.Apply, List[SyntheticImplicitParameter]]) = {
    var explicitSymbols : List[Tree] = List[Tree]()
    var callsWithImplicitParameters : Map[Term.Apply, List[SyntheticImplicitParameter]] = Map[Term.Apply, List[SyntheticImplicitParameter]]()
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
          val implicitParams : List[SyntheticImplicitParameter] = syntheticImplicits.collect{
            case (pos, sym) if pos == end => {
              val objectName = sym.denotation.get.name
              val suitableDeclarations : List[Tree] = explicitSymbols.collect {
                case x: Defn.Val if x.pats.exists {
                  case pat: Pat.Var if pat.name.value == objectName => true
                  case _ => false
                } => x
              }
              if (suitableDeclarations.nonEmpty) {
                val declaration = suitableDeclarations(0)
                println(declaration)
                SyntheticImplicitParameter(sym, Some(declaration))
              } else {
                SyntheticImplicitParameter(sym, None)
              }
            }
          }(collection.breakOut)
          println(s"Implicit parameters when calling ${node.fun.syntax}: $implicitParams")

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

object Format {

  def reportCalls(ctx: RuleCtx, treeImplicits: List[Tree], syntheticImplicits: List[(Int, Symbol)], callsWithImplicitParameters: Map[Term.Apply, List[SyntheticImplicitParameter]]) : Unit = {
    for {call <- callsWithImplicitParameters} {
      val function = call._1
      println(s"Call with implicit parameters:")
      println(s"  Called function: ${formatLocation(function)}: ${call._1.syntax}")
      println(s"  Declaration: ${}")
      println(s"  Implicit parameters:")
      for {param <- call._2} {
        println(s"    ${param.syntheticSymbol.syntax}${param.objectDeclaration match {case Some(tree) => s", declared in ${formatLocation(tree)}: $tree" case None => ""}}")
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

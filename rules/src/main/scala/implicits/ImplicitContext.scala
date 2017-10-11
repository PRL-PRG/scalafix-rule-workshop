package implicits

import scala.meta._
import scala.meta.contrib._
import scalafix.lint.{LintCategory, LintSeverity}
import scalafix.syntax._
import scalafix.rule.RuleCtx
import scalafix.{LintMessage, Patch, SemanticRule, SemanticdbIndex}

case class SyntheticImplicitParameters(syntheticSymbol: Synthetic, objectDeclaration: Option[Tree])

case class Location(line: Int, col: Int, sourceFile: String)
case class Declaration(name: String, location: Location)

trait ReportFormatter {
  def startReport(): String
  def endReport(): String

  def formatLocation(location: Location): String

  def startCallReport(): String
  def endCallReport(): String

  def reportFunctionName(name: String, location: Location): String
  def reportFunctionDeclaration(): String
  def startImplicitParameterReport(): String
  def endImplicitParamterReport(): String
  def reportImplicitParameter(name: String, declaration: Option[Declaration]): String
}

class HumanReadableFormatter() extends ReportFormatter {

  override def startReport(): String = "Analysis report--------------------\n"
  override def endReport(): String = "-------------------------\n"

  override def formatLocation(location: Location): String = s"[l:${location.line},c:${location.col}]@${location.sourceFile}"

  override def startCallReport(): String = "Call with implicit parameters:\n"
  override def endCallReport(): String = "\n"

  override def reportFunctionName(name: String, location: Location): String = s"  Called Function Name: ${formatLocation(location)}: ${name}\n"
  override def reportFunctionDeclaration(): String = s"  Declaration: \n"
  override def startImplicitParameterReport(): String = s"  Implicit Parameters:\n"
  override def endImplicitParamterReport(): String = s""

  override def reportImplicitParameter(name: String, declaration: Option[Declaration]): String = {
    s"    ${name}${declaration match {case Some(decl) => s", declared in ${formatLocation(decl.location)}:${decl.name}" case None => ""}}"
  }
}

object Locations {
  def getLocation(tree: Tree): Location = {
    Location(tree.pos.startLine, tree.pos.startColumn, getFileName(tree.input))
  }

  def getLocation(denot: ResolvedName): Location = {
    Location(denot.position.startLine, denot.position.startColumn, getFileName(denot.position.input))
    //s"${getFileName(denot.position.input)}@[l:${denot.position.startLine}, c:${denot.position.startColumn}]"
  }

  def getFileName(in: Input): String = {
    in match {
      case virtualFile: Input.VirtualFile => virtualFile.path
      case regularFile: Input.File => regularFile.path.toString()
      case synthetic: Input.Synthetic => "_Synthetic_"
      case _ => s"Input type Unknown.\n Here is the full Input: \n $in"
    }
  }
}

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

    implicit val formatter : ReportFormatter = new HumanReadableFormatter()
    var report = ""

    report += formatter.startReport()
    report += reportCalls(treeImplicits, syntheticImplicits, callsWithImplicitParameters)
    report += formatter.endReport()

    println(report)

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

  def reportCalls(treeImplicits: List[Tree], syntheticImplicits: List[(Int, Synthetic)], callsWithImplicitParameters: Map[Term.Apply, Synthetic])(implicit formatter: ReportFormatter) : String = {
    var report = ""

    report += formatter.startCallReport()
    for {call <- callsWithImplicitParameters} {
      val function = call._1
      val functionName = function.syntax
      val functionLocation = Locations.getLocation(function)
      report += formatter.reportFunctionName(functionName, functionLocation)
      report += formatter.reportFunctionDeclaration()
      report += formatter.startImplicitParameterReport()
      for {param <- call._2.names} {
        val paramName = param.symbol
        val paramDeclaration = getDeclaration(param)
        report += formatter.reportImplicitParameter(param.symbol.toString, paramDeclaration)
      }
      report += formatter.endImplicitParamterReport()
    }
    report += formatter.endCallReport()
    report
  }

  def getDeclaration(param: ResolvedName) : Option[Declaration] = {
    param.symbol.denotation match {
      case Some(denot) => Some(Declaration(readDenotation(denot), Locations.getLocation(param)))
      case None => None
    }
  }

  def readDenotation(denot: Denotation) : String = {
    denot.structure
  }
}


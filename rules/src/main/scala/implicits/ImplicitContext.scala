package implicits

import scala.meta._
import scala.meta.contrib._
import scalafix.lint.{LintCategory, LintSeverity}
import scalafix.syntax._
import scalafix.rule.RuleCtx
import scalafix.{LintMessage, Patch, SemanticRule, SemanticdbIndex}

// ---------------------------------------
// Representation of Analysis Results
// ---------------------------------------

case class CallsWithImplicitsReport(items: List[CallWithImplicits])
case class CallWithImplicits(function: CalledFunction, declaration: Option[FunctionDeclaration], parameters: List[ImplicitParameter])
case class CalledFunction(name: String, location: Location)
case class FunctionDeclaration(name: String, location: Location)
case class ImplicitParameter(name: String, declaration: Option[ImplicitParameterDeclaration])
case class ImplicitParameterDeclaration(name: String, location: Location)
case class Location(line: Int, col: Int, sourceFile: String)

// ---------------------------------------
// Report Formatters
// ---------------------------------------

trait ReportFormatter {
  def startReport(): String
  def endReport(): String
  def formatLocation(location: Location): String

  def reportCallsWithImplicits(report: CallsWithImplicitsReport): String
}



class HumanReadableFormatter() extends ReportFormatter {
  override def startReport(): String = "Analysis report--------------------\n"
  override def endReport(): String = "------------------------------------\n"
  override def formatLocation(location: Location): String = s"[l:${location.line},c:${location.col}]@${location.sourceFile}"

  override def reportCallsWithImplicits(report: CallsWithImplicitsReport): String = {
    var res = ""
    for {call <- report.items} {
      res += s"""Call with implicit parameters:
                |  Called Function Name: ${formatLocation(call.function.location)}: ${call.function.name}
                |  Declaration:
                |  Implicit Parameters:\n""".stripMargin
      for {param <- call.parameters} {
        res += s"    ${param.name}${param.declaration match {case Some(decl) => s", declared in ${formatLocation(decl.location)}:${decl.name}" case None => ""}}\n"
      }
    }
    res
  }
}

class JSONFormatter() extends ReportFormatter {
  override def startReport(): String = {
    indentation = 0
    """JSON Report ==================
      |{
  |""".stripMargin
  }
  override def endReport(): String = "}\n============================"
  override def formatLocation(location: Location): String = {
    formatLocation(location, false)
  }

  override def reportCallsWithImplicits(report: CallsWithImplicitsReport): String = {
    var res = StringBuilder.newBuilder
    indentForward()
    for {call <- report.items} {
      res.append(s"""$indent\"call_with_implicit_params\": {\n""")
      indentForward()
      formatCalledFunction(res, call.function)
      res.append(s"""$indent\"declaration\": \"None yet\",\n""")
      formatImplicitParameters(res, call.parameters)
      indentBack()
    }
    res.append(s"$indent}\n")
    indentBack()
    res.toString
  }

  private var indentation = 0
  private def indent(): String = " " * indentation
  private def indentForward() = indentation += 2
  private def indentBack() = indentation = Math.max(0, indentation - 2)

  private def formatLocation(location: Location, commaAtTheEnd: Boolean) : String = {
    var res = ""
    res += s"""$indent\"location\": {\n"""
    indentForward()
    res += s"""$indent\"line\": ${location.line},\n"""
    res += s"""$indent\"col\": ${location.col},\n"""
    res += s"""$indent\"sourceFile\": \"${location.sourceFile}\"\n"""
    indentBack()
    res += s"""$indent}${if (commaAtTheEnd) {","} else {""}}\n"""
    res
  }
  private def formatCalledFunction(res: StringBuilder, function: CalledFunction) = {
    res.append(s"""$indent\"function\": {\n""")
    indentForward()
    res.append(s"""$indent\"name\": \"${function.name}\",\n""")
    res.append(formatLocation(function.location, false))
    indentBack()
    res.append(s"""$indent},\n""")
  }

  private def formatImplicitParameters(res: StringBuilder, parameters: List[ImplicitParameter]) = {
    res.append(s"""$indent\"implicit_parameters\": [\n""")
    indentForward()
    var curParam = 0
    for {param <- parameters} {
      curParam += 1
      formatImplicitParameter(res, param, curParam != parameters.length)
    }
    indentBack()
    res.append(s"""${indent()}]\n""")
  }

  private def formatImplicitParameter(res: StringBuilder, param: ImplicitParameter, needsComma: Boolean) = {
    res.append(s"""$indent{ \n""")
    indentForward()
    res.append(s"""$indent\"name\": \"${param.name}\"""")
    param.declaration match {
      case Some(decl) => {
        res.append(s""",\n""")
        formatParameterDeclaration(res, decl)
      }
      case None => {
        res.append(s"$indent\n")
      }
    }
    indentBack()
    res.append(s"""$indent}${if (needsComma) {","} else {""}}\n""")
  }

  private def formatParameterDeclaration(res: StringBuilder, decl: ImplicitParameterDeclaration) = {
    res.append(s"""$indent\"declaration\": {\n""")
    indentForward()
    res.append(s"""$indent\"name\": \"${decl.name}\",\n""")
    res.append(formatLocation(decl.location, false))
    indentBack()
    res.append(s"""$indent}\n""")
  }
}

object Locations {
  def getLocation(tree: Tree): Location = {
    Location(tree.pos.startLine, tree.pos.startColumn, getFileName(tree.input))
  }

  def getLocation(denot: ResolvedName): Location = {
    Location(denot.position.startLine, denot.position.startColumn, getFileName(denot.position.input))
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

// ---------------------------------------
// Scalafix Rule
// ---------------------------------------

final case class ImplicitContext(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitContext")  {

  override def fix(ctx: RuleCtx): Patch = {
    println(s"Symbol collection -------------------------")

    val syntheticImplicits: List[(Int, Synthetic)] = collectSyntheticImplicits(ctx)
    //println(s"Synthetic Implicits: ${syntheticImplicits}")
    val (treeImplicits, callsWithImplicitParameters) = processExplicitTree(ctx, syntheticImplicits)
    //println(s"Explicit Symbols: ${treeImplicits}")
    //println(s"Calls With Implicit Parameters: ${callsWithImplicitParameters}")

    val formatter : ReportFormatter = new JSONFormatter()
    var report = ""

    report += formatter.startReport()
    report += formatter.reportCallsWithImplicits(reportCalls(treeImplicits, syntheticImplicits, callsWithImplicitParameters))
    report += formatter.endReport()

    println(report)

    Patch.empty
  }

  def collectSyntheticImplicits(ctx: RuleCtx) = {
    var syntheticImplicits: List[(Int, Synthetic)] = List[(Int, Synthetic)]()
    ProgressBar.setup("Collecting Synthetic Implicits", ctx.index.database.synthetics.length)
    for {synth <- ctx.index.database.synthetics} {
      for {name <- synth.names} {
        name.symbol.denotation match {
          case Some(denotation) => if (denotation.isImplicit) {
            syntheticImplicits = syntheticImplicits ++ List((synth.position.start, synth))
          }
          case None => {}
        }
      }
      ProgressBar.step()
    }
    syntheticImplicits
  }

  def processExplicitTree(ctx: RuleCtx, syntheticImplicits:  List[(Int, Synthetic)]) : (List[Tree], Map[Term.Apply, Synthetic]) = {
    var explicitSymbols : List[Tree] = List[Tree]()
    var callsWithImplicitParameters : Map[Term.Apply, Synthetic] = Map[Term.Apply, Synthetic]()
    UnboundedProgressDisplay.setup("Parsing Syntax Tree", 10)
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
      UnboundedProgressDisplay.step()
    }
    UnboundedProgressDisplay.close()
    (explicitSymbols, callsWithImplicitParameters)
  }

  def reportCalls(treeImplicits: List[Tree], syntheticImplicits: List[(Int, Synthetic)], callsWithImplicitParameters: Map[Term.Apply, Synthetic]) : CallsWithImplicitsReport = {
    var report = ""

    var calls = List[CallWithImplicits]()

    ProgressBar.setup("Analyzing Calls With Implicits", callsWithImplicitParameters.size)
    for {call <- callsWithImplicitParameters} {
      val function = call._1
      val functionName = function.syntax
      val functionLocation = Locations.getLocation(function)
      val calledFunction = CalledFunction(functionName, functionLocation)

      var parameterList = List[ImplicitParameter]()
      for {param <- call._2.names} {
        val paramName = param.symbol
        val paramDeclaration = getDeclaration(param)
        parameterList = parameterList ++ List(ImplicitParameter(paramName.toString, paramDeclaration))
      }
      calls = calls ++ List(CallWithImplicits(calledFunction, None, parameterList))
      ProgressBar.step()
    }
    CallsWithImplicitsReport(calls)
  }

  def getDeclaration(param: ResolvedName) : Option[ImplicitParameterDeclaration] = {
    param.symbol.denotation match {
      case Some(denot) => Some(ImplicitParameterDeclaration(readDenotation(denot), Locations.getLocation(param)))
      case None => None
    }
  }

  def readDenotation(denot: Denotation) : String = {
    denot.name
  }
}

// ---------------------------------------
// Utilities
// ---------------------------------------

object ProgressBar {
  var total = 0
  val barLength = 31
  var currentValue = 0

  def setup(prompt: String, totalValue: Int) = {
    print(s"${prompt}: [${" " * barLength}]")
    total = totalValue
    currentValue = 0
    (0 to barLength).foreach(_ => print("\b"))
  }

  def step() = {
    currentValue += 1
    val accomplished = currentValue.toFloat / total.toFloat
    val dots = Math.floor(accomplished * barLength).toInt
    (1 to dots) foreach {_ => print("=")}
    (1 to (barLength - dots)).foreach(_ => print(" "))
    print("] " + s"${Math.round(accomplished * 100)}%")
    if (currentValue == total) {
      println(" Done!")
    } else {
      (1 to barLength + 5).foreach(_ => print("\b"))
    }
  }
}

object UnboundedProgressDisplay {
  var updateCounter = 0
  var updateInterval = 0
  var symbol = ""

  def setup(prompt: String, interval: Int, progressSymbol: String = ".") = {
    print(s"${prompt}: ")
    updateInterval = interval
    symbol = progressSymbol
  }

  def step() = {
    updateCounter += 1
    if (updateCounter == updateInterval) {
      print(symbol)
      updateCounter = 0
    }
  }

  def close() = {
    println(" Done!")
  }
}

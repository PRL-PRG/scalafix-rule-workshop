package implicits

import scala.collection.mutable
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
case class CalledFunction(name: String, location: Location, declaration: Option[FunctionDeclaration])
case class FunctionDeclaration(name: String, location: Location)
case class ImplicitParameter(name: String, declaration: Option[ImplicitParameterDeclaration])
case class ImplicitParameterDeclaration(name: String, location: Location)
case class Location(line: Int, col: Int, sourceFile: String, imported: Boolean)

class RawCallChains {
  var chains = collection.mutable.Map[Term, List[Term]]()
  var curTerm: Term = Term.fresh()
  var inAChain: Boolean = false
  def openIfNecessary(first: Term) = {
    if (!inAChain) {
      curTerm = first
      chains = chains ++ Map(first -> List[Term]())
      inAChain = true
    }
  }

  def close(): Unit = {
    inAChain = false
  }
  def insert(term: Term) = {
    chains(curTerm) = chains(curTerm) ++ List(term)
  }
  override def toString = s"CallChains($chains)"
}
case class CallChainGraph()
case class CallChain(calls: List[String])
case class CallChainNode(param: String, calls: List[CallChain])
class CallChainRepository() {
  var data = mutable.Map[String, mutable.MutableList[(mutable.MutableList[String], String)]]()
  private var currentChains = mutable.Map[String, Int]()
  def insertIntoLatestChainFor(implObject: String, call: String): Unit = {
    data(implObject)(currentChains(implObject))._1 += call
  }
  def openNewChainFor(implObject: String, file: String): Unit = {
    currentChains(implObject) += 1
    data(implObject).+=((mutable.MutableList[String](), file))
  }
  def insertNewImplicitObjectMaybe(implObject: String): Unit = {
    if (!currentChains.contains(implObject)) {
      currentChains(implObject) = -1
      data(implObject) = mutable.MutableList[(mutable.MutableList[String], String)]()
    }
  }

  override def toString = s"CallChainRepository($data)"
}



// ---------------------------------------
// Report Formatters
// ---------------------------------------

trait ReportFormatter {
  def startReport(): String
  def endReport(): String
  def formatLocation(location: Location): String

  def reportCallsWithImplicits(report: CallsWithImplicitsReport): String
  def reportCallChains(chainRepo: CallChainRepository): String
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

  override def reportCallChains(chainRepo: CallChainRepository) = {
    var res = ""
    for {synthetic <- chainRepo.data} {
      res += s"Call chains where ${synthetic._1} is passed as a parameter:\n"
      var chainNum = 0
      for {chain <- synthetic._2} {
        res += s"  [${chainNum}]: "
        for {func <- chain._1} {
          res += s"${func}."
        }
        res += s"@${chain._2}\n"
        chainNum += 1
      }
    }
    res
  }
}

class JSONFormatter() extends ReportFormatter {
  override def startReport(): String = {
    indentation = 0
    """JSON Report ==================
  |""".stripMargin
  }
  override def endReport(): String = "============================"
  override def formatLocation(location: Location): String = {
    formatLocation(location, false)
  }

  override def reportCallsWithImplicits(report: CallsWithImplicitsReport): String = {
    var res = StringBuilder.newBuilder
    res.append(s"""$indent\"calls\": [\n""")
    indentForward()
    var callNum = 0
    for {call <- report.items} {
      res.append(s"""$indent\"call_with_implicit_params\": {\n""")
      indentForward()
      formatCalledFunction(res, call.function)
      formatImplicitParameters(res, call.parameters)
      indentBack()
      callNum += 1
      res.append(s"$indent}${if (callNum != report.items.size) {","} else {""}}\n")
    }
    indentBack()
    res.append(s"$indent]\n")
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
    res += s"""$indent\"sourceFile\": \"${location.sourceFile}\",\n"""
    res += s"""$indent\"imported\": \"${location.imported}\"\n"""
    indentBack()
    res += s"""$indent}${if (commaAtTheEnd) {","} else {""}}\n"""
    res
  }
  private def formatCalledFunction(res: StringBuilder, function: CalledFunction) = {
    res.append(s"""$indent\"function\": {\n""")
    indentForward()
    res.append(s"""$indent\"name\": \"${function.name}\",\n""")
    res.append(s"""$indent\"declaration\": \"None yet\",\n""")
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

  override def reportCallChains(chainRepo: CallChainRepository) = {
    var res = StringBuilder.newBuilder
    res.append(s"""$indent\"call_chains\": [\n""")
    var synthNum = 0
    indentForward()
    var callNum = 0
    for {synthetic <- chainRepo.data} {
      res.append(s"""$indent\"call_chain\": {\n""")
      indentForward()
      res.append(s"""$indent\"synthetic_object\": \"${synthetic._1}\",\n""")
      res.append(s"""$indent\"calls: \"[\n""")
      indentForward()
      var chainNum = 0
      for {chain <- synthetic._2} {

        var chainString = ""
        for {func <- chain._1} {
          chainString += s"${func}."
        }
        chainString += s"@${chain._2}"
        res.append(s"""$indent\"chain\": \"${chainString}\"${if (chainNum != synthetic._2.size) {","} else {""}}\n""")
        chainNum += 1
      }
      indentBack()
      res.append(s"$indent]\n")

      indentBack()
      synthNum += 1
      res.append(s"$indent}${if (synthNum != chainRepo.data.size) {","} else {""}}\n")
    }
    indentBack()
    res.append(s"$indent]\n")
    res.toString
  }
}

object Locations {
  def getLocation(tree: Tree): Location = {
    Location(tree.pos.startLine, tree.pos.startColumn, getFileName(tree.input), false)
  }

  def getLocation(denot: ResolvedName): Location = {
    Location(denot.position.startLine, denot.position.startColumn, getFileName(denot.position.input), false)
  }

  def getFileName(in: Input): String = {
    in match {
      case virtualFile: Input.VirtualFile => virtualFile.path
      case regularFile: Input.File => regularFile.path.toString()
      case synthetic: Input.Synthetic => s"(_Synthetic_)${getFileName(synthetic.input)}"
      case _ => s"Input type Unknown.\n Here is the full Input: \n $in"
    }
  }

  def getFunctionName(syntax: String) : String = {
    syntax.replaceAll("\\s+","").replaceAll("\"", "'").replaceAll("\\(.*?\\)", "()")
  }
}

// ---------------------------------------
// Scalafix Rule
// ---------------------------------------

final case class ImplicitContext(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitContext")  {

  override def fix(ctx: RuleCtx): Patch = {
    Timer.time {
      val syntheticImplicits: List[(Int, Synthetic)] = collectSyntheticImplicits(ctx)
      //println(s"Synthetic Implicits: ${syntheticImplicits}")
      val (treeImplicits, callsWithImplicitParameters, callChains) = processExplicitTree(ctx, syntheticImplicits)
      //println(s"Explicit Symbols: ${treeImplicits}")
      //println(s"Calls With Implicit Parameters: ${callsWithImplicitParameters}")
      println(s"Call chains: ${callChains}")

      val chainRepo = reportCallChains(callsWithImplicitParameters, callChains)


      val formatter: ReportFormatter = new HumanReadableFormatter()
      var report = ""

      report += formatter.startReport()
      report += formatter.reportCallsWithImplicits(reportCalls(treeImplicits, syntheticImplicits, callsWithImplicitParameters))
      report += formatter.reportCallChains(chainRepo)
      report += formatter.endReport()

      println(report)
    }

    Patch.empty
  }

  def collectSyntheticImplicits(ctx: RuleCtx) = {
    var syntheticImplicits: List[(Int, Synthetic)] = List[(Int, Synthetic)]()
    UnboundedProgressDisplay.setup("Collecting Synthetic Implicits")
    for {synth <- ctx.index.database.synthetics} {
      for {name <- synth.names} {
        name.symbol.denotation match {
          case Some(denotation) => if (denotation.isImplicit) {
            syntheticImplicits = syntheticImplicits ++ List((synth.position.start, synth))
          }
          case None => {}
        }
      }
      UnboundedProgressDisplay.step()
    }
    UnboundedProgressDisplay.close()
    syntheticImplicits
  }

  def processExplicitTree(ctx: RuleCtx, syntheticImplicits:  List[(Int, Synthetic)]) : (List[Tree], Map[Term, Synthetic], RawCallChains) = {
    var explicitSymbols : List[Tree] = List[Tree]()
    var callsWithImplicitParameters : Map[Term, Synthetic] = Map[Term, Synthetic]()

    var callChains = new RawCallChains()

    UnboundedProgressDisplay.setup("Parsing Syntax Tree Nodes")
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
          var insertable: Term = node
          node.fun match {
            case fun: Term.Select => {
              insertable = fun.name
              callChains.openIfNecessary(node)
              callChains.insert(insertable)
            }
            case fun: Term.Name => {
              insertable = fun
              // Goes into all the selects first, then to the names
              callChains.insert(insertable)
              callChains.close()
            }
            case fun: Term.Apply => {
              callChains.openIfNecessary(node)
              callChains.insert(fun)
            }
            case _ => {
            }
          }
          syntheticImplicits.find { _._1 ==  end} match {
            case Some(synthetic) => {
              callsWithImplicitParameters = callsWithImplicitParameters ++ Map(insertable -> synthetic._2)
            }
            case None => None
          }
        }
        case _ => {}
      }
      UnboundedProgressDisplay.step()
    }
    UnboundedProgressDisplay.close()
    (explicitSymbols, callsWithImplicitParameters, callChains)
  }

  def reportCalls(treeImplicits: List[Tree], syntheticImplicits: List[(Int, Synthetic)], callsWithImplicitParameters: Map[Term, Synthetic]) : CallsWithImplicitsReport = {
    var report = ""

    var calls = List[CallWithImplicits]()

    UnboundedProgressDisplay.setup("Analyzing Calls With Implicits")
    for {call <- callsWithImplicitParameters} {
      val function = call._1
      val functionName = Locations.getFunctionName(function.syntax)
      val functionLocation = Locations.getLocation(function)
      val calledFunction = CalledFunction(functionName, functionLocation, None)

      var parameterList = List[ImplicitParameter]()
      for {param <- call._2.names} {
        val paramName = param.symbol
        if (paramName.syntax != "_star_.") {
          val paramDeclaration = getDeclaration(param)
          parameterList = parameterList ++ List(ImplicitParameter(paramName.toString, paramDeclaration))
        }
      }
      calls = calls ++ List(CallWithImplicits(calledFunction, None, parameterList))
      UnboundedProgressDisplay.step()
    }
    UnboundedProgressDisplay.close()
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

  def reportCallChains(callsWithImplicitParameters: Map[Term, Synthetic], chains: RawCallChains) : CallChainRepository = {
    val repo = new CallChainRepository()
    for {chain <- chains.chains} {
      val firstCall = chain._2.head
      val firstCallSynthetic = callsWithImplicitParameters(chain._2.head)
      for {implObj <- firstCallSynthetic.names} {
        repo.insertNewImplicitObjectMaybe(implObj.symbol.syntax)
        repo.openNewChainFor(implObj.symbol.syntax, Locations.getFileName(firstCall.input))
      }
      for {call <- chain._2} {
        for {implParam <- callsWithImplicitParameters(call).names} {
          repo.insertIntoLatestChainFor(implParam.symbol.toString, s"${call.syntax}()")
        }
      }
    }
    repo
   }
}

// ---------------------------------------
// Utilities
// ---------------------------------------

object UnboundedProgressDisplay {
  var updateCounter = 0
  var numberLength = 0

  def setup(prompt: String) = {
    print(s"${prompt}: ")
    numberLength = 0
    updateCounter = 0
  }

  def step() = {
    updateCounter += 1
    val newNumber = updateCounter.toString
    (1 to numberLength) foreach { _ => print("\b") }
    print(newNumber)
    numberLength = newNumber.length
  }

  def close() = {
    println(" Done!")
  }
}

object Timer {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
}
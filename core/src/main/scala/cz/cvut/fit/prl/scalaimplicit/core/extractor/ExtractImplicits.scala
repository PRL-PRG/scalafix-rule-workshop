package cz.cvut.fit.prl.scalaimplicit.core.extractor

import cz.cvut.fit.prl.scalaimplicit.core.extractor.Serializables.{
  Apply,
  DeclaredImplicit,
  FunApplyWithImplicitParam,
  ImplicitParam
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  Factories,
  Gatherer,
  ReflectiveCtx,
  SemanticCtx
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.{
  CallSite,
  Declaration
}
import sext._

import scala.meta._
import scala.reflect.runtime.{universe => u}

case class TArg(symbol: Symbol, args: Seq[TArg])
case class QualifiedSymbol(app: Option[Symbol],
                           isSynthetic: Boolean,
                           pos: Option[Position] = None)
object QualifiedSymbol {
  val Empty = QualifiedSymbol(None, false)
}
case class BreakDown(symbol: QualifiedSymbol,
                     typeParams: Seq[TArg],
                     params: Seq[BreakDown])

/**
  * Common interface for objects that decompose terms
  *
  * Walking a term tree to form a useful form of a function
  * is a tedious process. This has to be done twice -
  * Once on the synthetics to examine the synthetic itself,
  * and another one once we have matched the synthetics with their missing applications.
  * This means that the only difference is in how they find symbols,
  * which is this accepts a function.
  *
  * The accepted function is implicit for convenience, since breakDown and
  * processType are recursive and implicit functions simplify the calls.
  */
object TermDecomposer {
  def apply(tree: Term, finder: Tree => QualifiedSymbol): BreakDown = {
    breakDown(tree)(finder)
  }

  private def processType(tree: Type)(
      implicit finder: Tree => QualifiedSymbol): TArg = {
    tree match {
      case t: Type.Apply => {
        val pt = processType(t.tpe)
        val targs = t.args.map(processType)
        TArg(pt.symbol, targs)
      }
      case t: Type.Name => {
        val symbol = finder(t).app.get
        TArg(symbol, Seq())
      }
    }
  }

  private def breakDown(tree: Term)(
      implicit finder: Tree => QualifiedSymbol): BreakDown = {
    def processParamList(params: Seq[Term]): Seq[BreakDown] =
      params.map(breakDown).filter(_.symbol.app.isDefined)

    tree match {
      case t: Term.Apply => {
        // Anything with a parameter list (`hello(*)`, `hello[String](*)`, `hello[String](*)(stringConverter)`...)
        val bd = breakDown(t.fun)
        bd.copy(params = processParamList(t.args))
      }
      case t: Term.ApplyType => {
        // An application with type parameters but no parameter list
        // examples: `test.this.JsonWriter[Seq[Student]]`
        val bd = breakDown(t.fun)
        val targs = t.targs.map(processType)
        bd.copy(typeParams = targs)
      }
      case t: Term.Select => {
        // Does not have parameters (otherwise it would be a Term.Apply) or type parameters (Term.ApplyType)
        // examples: `test.this.JsonWriter`
        breakDown(t.name)
      }
      case t: Term.Name => {
        // Plain name of the symbol we want (e.g. in `test.this.JsonWriter` -> `"JsonWriter"`)
        val app = finder(t)
        BreakDown(app, Seq(), Seq())
      }
      case t: Term.Block => {
        // A block inside the synthetic (e.g. `nested.this.a2c(*)({((a: A) => nested.this.a2b(a))})`)
        // We assume it has only one stat inside, and that it is a Term.
        assert(t.stats.size == 1,
               s"Body ${t.stats} of block $t has more than one stat")
        assert(t.stats.forall(_.isInstanceOf[Term]),
               s"Stat (${t.stats.head}) from block ${t} is not a term")
        breakDown(t.stats.head.asInstanceOf[Term])
      }
      case t: Term.Function => {
        // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
        // We assume that the body is a single function call, as is the most typical in passing parameters
        // We also ignore the parameters for now, since they will appear in other call sites,
        // when the function gets executed
        breakDown(t.body)
      }
      case t: Term.ApplyInfix => {
        // Infix function applications (e.g. `list map f1`).
        // We take the operation and the ''implicit'' parameters
        val bd = breakDown(t.op)
        bd.copy(params = processParamList(t.args))
      }
      case t: Term.ApplyUnary => {
        // Unary functions (e.g. )
        val bd = breakDown(t.op)
        bd.copy(params = processParamList(Seq(t.arg)))
      }
      case t: Term.New => {
        // Class constructor calls.
        // If the constructor is anonymous, we return the symbol of the class.
        // FIXME: This assumes that we won't find this in a synthetic
        val app = t.init.name match {
          case n: Name.Anonymous => finder(t.init.tpe)
        }
        BreakDown(app, Seq(), Seq())
      }
    }
  }
}

object Queries {

  def breakDownSynthetic(ctx: SemanticCtx, synth: Synthetic): BreakDown = {

    def parse(text: String): Term = text.parse[Term].get

    def finder(tree: Tree): QualifiedSymbol = {
      synth.names.find(_.position.end == tree.pos.end) match {
        // Filter out the _star_ names
        case Some(n) if n.symbol.syntax.contains("_star_") =>
          QualifiedSymbol.Empty
        case Some(name) =>
          QualifiedSymbol(Some(name.symbol),
                          isSynthetic = true,
                          Some(synth.position))
        case None => QualifiedSymbol.Empty
      }
    }

    def assertWeCanEraseParams(application: BreakDown) = {
      // Assert that no parameter of the matched application is implicit.
      // This way, we ensure that we are not losing information when replacing
      // that parameter list with the synthetic one
      assert(
        application.params.isEmpty || application.params.forall(x =>
          !ctx.denotation(x.symbol.app.get).get.isImplicit),
        s"Some parameter of matched application ${application} is implicit"
      )
    }

    val processedSynthetic = TermDecomposer(parse(synth.text), finder)
    val res = processedSynthetic.symbol.app match {
      case Some(app) => processedSynthetic
      case None => {
        val matchedApplication = findApplication(ctx, synth)
        assertWeCanEraseParams(matchedApplication)
        matchedApplication.copy(params = processedSynthetic.params)
      }
    }
    assert(
      res.symbol.app.isDefined,
      s"Couldn't find an application for synthetic ${synth.text}"
    )
    res
  }

  /**
    * Find applications for the synthetics that don't have them (that is, pure parameter lists)
    * For that, we try to look for a fitting `apply` synthetic. If we don't find one, we look in
    * the source code and try to match there.
    *
    * In both cases, we match by position, since parameter lists are inserted at the end of calls.
    *
    * We assume that there is exactly one symbol at the position of the synthetic.
    */
  def findApplication(ctx: SemanticCtx, synth: Synthetic): BreakDown = {

    def breakdownTree(term: Option[Tree]): BreakDown = {
      def finder(t: Tree): QualifiedSymbol = {
        // A symbol from the tree will never be synthetic
        QualifiedSymbol(
          Some(
            ctx
              .symbol(t)
              .getOrElse(Symbol(ctx.qualifiedName(t.asInstanceOf[Term])))),
          isSynthetic = false,
          pos = Some(t.pos)
        )
      }

      term match {
        case Some(t) => TermDecomposer(t.asInstanceOf[Term], finder)
        case None => BreakDown(QualifiedSymbol.Empty, Seq(), Seq())
      }
    }

    ctx.syntheticApplication(synth.position.end) match {
      // There is a synthetic application that matches
      case Some(syntheticApply) => breakDownSynthetic(ctx, syntheticApply)
      // Parse from the tree itself
      case None => breakdownTree(ctx.inSourceCallSite(synth.position.end))
    }
  }

  case class ReflectiveTArg(symbol: u.Symbol, args: Seq[ReflectiveTArg])
  case class ReflectiveBreakdown(originalSymbol: QualifiedSymbol,
                                 reflection: u.Symbol,
                                 params: Seq[ReflectiveBreakdown],
                                 typeArguments: Seq[ReflectiveTArg])

  /**
    * Query the context to fetch the reflective symbols of every relevant
    * scala.meta symbol in the synthetic
    * @param ctx
    * @param breakdown
    * @return
    */
  def getReflectiveSymbols(ctx: ReflectiveCtx,
                           breakdown: BreakDown): ReflectiveBreakdown = {

    def getReflectiveTArg(ctx: ReflectiveCtx, targ: TArg): ReflectiveTArg = {
      ReflectiveTArg(
        symbol = ctx.findReflectSymbol(targ.symbol),
        args = targ.args.map(getReflectiveTArg(ctx, _))
      )
    }

    val app = breakdown.symbol.app.getOrElse(throw new RuntimeException(
      s"Breakdown ${breakdown.symbol} has no application and reached reflection. This should never happen"))

    ReflectiveBreakdown(
      originalSymbol = breakdown.symbol,
      reflection = ctx.findReflectSymbol(app),
      params = breakdown.params.map(getReflectiveSymbols(ctx, _)),
      typeArguments = breakdown.typeParams.map(getReflectiveTArg(ctx, _))
    )
  }
}

case class ExtractionResult(callSites: Seq[CallSite],
                            declarations: Set[Declaration])
object ExtractionResult {
  val Empty = ExtractionResult(Seq(), Set())
}
object ReflectExtract extends (ReflectiveCtx => ExtractionResult) {

  def apply(ctx: ReflectiveCtx): ExtractionResult = {
    val callSites =
      ctx.syntheticsWithImplicits
        .map(Queries.breakDownSynthetic(ctx, _))
        .map(Queries.getReflectiveSymbols(ctx, _))
        .map(Factories.createCallSite(ctx, _))

    val declarations = Gatherer.gatherDeclarations(callSites)
    // Can be turned on for debugging if needed
    //println(res.treeString)
    //println(res.valueTreeString)
    ExtractionResult(callSites, declarations)
  }
}

// TODO Remove once the thing above is stable

import org.langmeta.inputs.Input

import scala.meta._

final case class Result(params: Set[ImplicitParam],
                        funs: Seq[Apply],
                        links: Set[FunApplyWithImplicitParam],
                        implicits: Set[DeclaredImplicit])
object Result {
  val Empty = Result(Set(), Seq(), Set(), Set())
}

object ExtractImplicits extends (SemanticCtx => Result) {

  final case class LinkPair(param: ImplicitParam, fun: Apply)

  def apply(ctx: SemanticCtx): Result = {
    val file: String = ctx.input match {
      case Input.VirtualFile(path, _) => path
      case Input.File(path, _) => path.toString
      case _ => ""
    }

    /**
      * Implicit parameters are inserted by the compiler.
      * This captures all implicits inserted,
      * to be matched later with function applications.
      */
    val syntheticImplicits: Map[Int, ImplicitParam] = Map((for {
      syn <- ctx.index.synthetics
      name <- syn.names
      symbol = name.symbol
      den <- ctx.denotation(symbol) if den.isImplicit
    } yield {
      syn.position.end -> Serializables.createImplicitParam(ctx, symbol, den)
    }): _*)

    /**
      * Filter for the synthetic function applications.
      * Captures all "apply()" functions inserted by the compiler.
      * @param elem Synthetic to test
      * @return true iff the synthetic has apply in the name
      */
    def hasApplyInTheName(elem: Synthetic): Boolean = {
      elem.names.exists(_.toString() == "apply")
    }

    val syntheticApplications =
      ctx.index.synthetics.filter(hasApplyInTheName)

    /**
      * Capture function applications, both in source and inserted by the compiler,
      * and match them with the implicit parameters captured earlier.
      */
    val paramsFuns =
      for {

        /**
          * We capture both Names and Applies that appear in-tree, since both represent function applications.
          *
          * In semanticdb notation, a Term.Name is the name part of a call.
          * In function chains, it may happen that we have only Term.Names (and not Term.Applies)
          * for individual functions in the chain. Thus, it is not sufficient to match Term.Applies.
          *
          * e.g.: https://astexplorer.net/#/gist/3246f2f332f71e73e4e0da969e8eed22/latest
          *
          * This first filter matches every call, both the outer Applies and the inner Names.
          * Later filters leave out those calls without implicit parameters.
          */
        app <- ctx.tree collect {
          case x: Term.Apply =>
            Serializables.AppTerm(x, x.args.size, x.fun.pos.end)
          case x: Term.Name => Serializables.AppTerm(x, 0, x.pos.end)
        }
        param <- syntheticImplicits.filter(_._1 == app.term.pos.end).values
      } yield {
        syntheticApplications
          .find { x =>
            x.position.end >= app.term.pos.start && x.position.end <= app.term.pos.end
          }
          .map { x =>
            LinkPair(
              param,
              Serializables.createSyntheticApply(ctx, x, file, app.params))
          }
          .getOrElse(
            LinkPair(param, Serializables.createFunctionApply(ctx, app, file)))

      }

    val params = paramsFuns.groupBy(_.param).keys.toSet
    val funs = paramsFuns.groupBy(_.fun).keys.toSeq
    val links =
      paramsFuns.map(x => Serializables.createLink(x.param, x.fun)).toSet

    /**
      * Collection of declaration of implicits in a file.
      * Accounts for all declarations with the implicit keyword,
      * be it parameters, vals, defs, objects...
      */
    val declaredImplicits =
      for {
        name <- ctx.index.names if name.isDefinition
        den <- ctx.denotation(name.symbol) if den.isImplicit
      } yield {
        Serializables.createDeclaredImplicit(ctx, name, den, file)
      }

    Result(params, funs, links, declaredImplicits.toSet)
  }
}

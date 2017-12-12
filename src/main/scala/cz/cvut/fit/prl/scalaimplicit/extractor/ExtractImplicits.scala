package cz.cvut.fit.prl.scalaimplicit.extractor

import cz.cvut.fit.prl.scalaimplicit.extractor.Queries.{
  ReflectiveBreakdown,
  ReflectiveTArg
}
import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.{Representation => r}
import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.{
  Factories,
  ReflectiveCtx,
  SemanticCtx
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
case class BreakdownContent(symbol: QualifiedSymbol,
                            typeParams: Seq[TArg],
                            params: Seq[BreakdownContent])
trait TermDecomposer {

  def findSymbolFor(term: Tree): QualifiedSymbol

  def processType(tree: Type): TArg = {
    tree match {
      case t: Type.Apply => {
        val pt = processType(t.tpe)
        val targs = t.args.map(processType)
        TArg(pt.symbol, targs)
      }
      case t: Type.Name => {
        val symbol = findSymbolFor(t).app.get
        TArg(symbol, Seq())
      }
    }
  }

  def breakDown(tree: Term): BreakdownContent = {
    def processParamList(params: Seq[Term]): Seq[BreakdownContent] =
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
        val app = findSymbolFor(t)
        BreakdownContent(app, Seq(), Seq())
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
    }
  }
}

object Queries {
  def syntheticsWithImplicits(ctx: SemanticCtx): Seq[Synthetic] =
    ctx.index.synthetics.filter(_.text.contains("("))

  case class SyntheticBreakdown(synth: Synthetic, content: BreakdownContent)

  def breakDownSynthetic(ctx: SemanticCtx,
                         synth: Synthetic): SyntheticBreakdown = {

    def parse(text: String): Term = {
      text.parse[Term].get
    }

    // We define it internally so that we have access to `synth`
    object internal extends TermDecomposer {
      override def findSymbolFor(t: Tree): QualifiedSymbol = {
        synth.names.find(_.position.end == t.pos.end) match {
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
    }

    def assertWeCanEraseParams(application: BreakdownContent) = {
      // Assert that no parameter of the matched application is implicit.
      // This way, we ensure that we are not losing information when replacing
      // that parameter list with the synthetic one
      assert(
        application.params.isEmpty || application.params.forall(x =>
          !ctx.denotation(x.symbol.app.get).get.isImplicit),
        s"Some parameter of matched application ${application} is implicit"
      )
    }

    val processedSynthetic = internal.breakDown(parse(synth.text))
    SyntheticBreakdown(
      synth = synth,
      content = processedSynthetic.symbol.app match {
        case Some(app) => processedSynthetic
        case None => {
          val matchedApplication = findApplication(ctx, synth)
          assertWeCanEraseParams(matchedApplication)
          matchedApplication.copy(params = processedSynthetic.params)
        }
      }
    )
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
  def findApplication(ctx: SemanticCtx, synth: Synthetic): BreakdownContent = {

    def breakdownTree(term: Option[Tree]): BreakdownContent = {
      object internal extends TermDecomposer {
        override def findSymbolFor(t: Tree): QualifiedSymbol = {
          // A symbol from the tree will never be synthetic
          QualifiedSymbol(
            Some(
              ctx
                .symbol(t)
                .getOrElse(Symbol(ctx.qualifiedName(t.asInstanceOf[Term])))),
            isSynthetic = false
          )
        }
      }
      term match {
        case Some(t) => internal.breakDown(t.asInstanceOf[Term])
        case None => BreakdownContent(QualifiedSymbol.Empty, Seq(), Seq())
      }
    }

    /**
      * We capture every instance of function applications
      * that may have implicit parameters.
      *
      * FIXME This list may not be exhaustive, but an assertion will be triggeded if a case is missed
      * There are some notable omissions, such as For and ForYield. The reason is that we
      * will not have symbols for them, and thus it is necessary to treat it as a special case
      *
      * ''NOTE'' That toMap will override entries with the same position
      */
    def inSourceCallSites(tree: Tree): Map[Int, Tree] =
      (tree collect {
        case x @ (_: Term.Apply | _: Term.ApplyInfix | _: Term.Select |
            _: Term.ApplyType | _: Term.ApplyUnary | _: Term.Interpolate) =>
          x.pos.end -> x
      }).toMap

    /**
      * Filter for the synthetic function applications.
      * Captures all "apply()" functions inserted by the compiler.
      *
      * @param elem Synthetic to test
      * @return true iff the synthetic has apply in the name
      */
    def hasApplyInTheName(elem: Synthetic): Boolean = {
      elem.text.startsWith("*.apply")
    }

    val syntheticApplies: Seq[SyntheticBreakdown] =
      ctx.index.synthetics
        .filter(hasApplyInTheName)
        .map(Queries.breakDownSynthetic(ctx, _))

    val inSource = inSourceCallSites(ctx.tree)
    syntheticApplies.find(_.synth.position.end == synth.position.end) match {
      // There is a synthetic application that matches
      case Some(callSite) => callSite.content
      // Parse from the tree itself
      case None => breakdownTree(inSource.get(synth.position.end))
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
  def getReflectiveSymbols(
      ctx: ReflectiveCtx,
      breakdown: BreakdownContent): ReflectiveBreakdown = {

    /**
      * Apply some heuristic to select one type symbol of the many that there can be
      * Currently, the heuristic is trait > class > case class > object > package
      * @param symbols
      * @return
      */
    def selectTypeSymbol(symbols: Set[u.Symbol]): u.Symbol = {
      val priorities = Map("trait" -> 0,
                           "class" -> 1,
                           "case class" -> 2,
                           "object" -> 3,
                           "case object" -> 4,
                           "package" -> 5)
      symbols.toSeq
        .sortWith((a, b) => {
          priorities
            .find(x => a.toString.startsWith(x._1))
            .get
            ._2 < priorities.find(x => b.toString.startsWith(x._1)).get._2
        })
        .head
    }

    /**
      * Same as selectTypeSymbol, but for non-types.
      * FIXME: Currently it only takes the head symbol.
      * @param symbols
      * @return
      */
    def selectTermSymbol(symbols: Set[u.Symbol]): u.Symbol = {
      symbols.filter(_.isTerm).head
    }

    def getReflectiveTArg(ctx: ReflectiveCtx, targ: TArg): ReflectiveTArg = {
      ReflectiveTArg(
        symbol = selectTypeSymbol(ctx.fetchReflectSymbol(targ.symbol)),
        args = targ.args.map(getReflectiveTArg(ctx, _))
      )
    }

    val app = breakdown.symbol.app.getOrElse(throw new RuntimeException(
      s"Breakdown ${breakdown.symbol} has no application and reached reflection. This should never happen"))

    ReflectiveBreakdown(
      originalSymbol = breakdown.symbol,
      reflection = selectTermSymbol(ctx.fetchReflectSymbol(app)),
      params = breakdown.params.map(getReflectiveSymbols(ctx, _)),
      typeArguments = breakdown.typeParams.map(getReflectiveTArg(ctx, _))
    )
  }
}

object ReflectExtract extends (ReflectiveCtx => Seq[r.TopLevelElem]) {

  def apply(ctx: ReflectiveCtx): Seq[r.TopLevelElem] = {
    val implicits = Queries.syntheticsWithImplicits(ctx)
    val breakDowns = implicits.map(Queries.breakDownSynthetic(ctx, _))
    assert(
      !breakDowns.exists(_.content.symbol.app.isEmpty),
      s"Some synthetic(s) didn't find an application: ${breakDowns.filter(_.content.symbol.app.isEmpty).mkString("\n")}"
    )

    val reflections =
      breakDowns.map(x => Queries.getReflectiveSymbols(ctx, x.content))
    val res = reflections.map(x => Factories.createCallSite(ctx, x))
    println(res.treeString)
    //println(res.valueTreeString)
    res
  }
}

// TODO Remove once the thing above is stable

import cz.cvut.fit.prl.scalaimplicit.extractor.Serializables.{
  Apply,
  DeclaredImplicit,
  FunApplyWithImplicitParam,
  ImplicitParam
}
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

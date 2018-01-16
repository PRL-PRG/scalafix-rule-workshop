package cz.cvut.fit.prl.scalaimplicit.core.extractor

import cz.cvut.fit.prl.scalaimplicit.core.extractor.Serializables.{
  Apply,
  DeclaredImplicit,
  FunApplyWithImplicitParam,
  ImplicitParam
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.{
  CallSite,
  Declaration
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts._

import scala.meta._

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

  def processParamList(params: Seq[Term])(
      implicit finder: Tree => QualifiedSymbol): Seq[Param] = {
    def getParameter(term: Tree): Param = term match {
      case t: Term.Assign => {
        RawCode(t.syntax, t.pos)
      }
      case t: Term.Block => {
        // A block inside the synthetic (e.g. `nested.this.a2c(*)({((a: A) => nested.this.a2b(a))})`)
        // We assume it has only one stat inside, and that it is a Term.
        // If it has more than one stat, we just regard it as a parameter
        if (t.stats.size == 1) {
          assert(t.stats.forall(_.isInstanceOf[Term]),
                 s"Stat (${t.stats.head}) from block ${t} is not a term")
          getParameter(t.stats.head.asInstanceOf[Term])
        } else {
          RawCode(t.syntax, t.pos)
        }
      }
      case t: Term.Function => {
        // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
        // If the function appears in a parameter list, it is safe to consider it
        // as raw code, since implicit defs passed as implicit parameters will be passed in blocks
        // (See `case t: Term.Block`)
        t.body match {
          case application if SemanticCtx.isApplication(application) =>
            breakDown(application)
          case body => RawCode(t.syntax, t.pos)
        }
      }
      case t: Term.PartialFunction => {
        // A generated partial function application (e.g. `{ case (_, v) => v.head }`)
        RawCode(t.syntax, t.pos)
      }
      case t: Term.Ascribe => {
        // Type ascriptions: `((ClassTag.apply[String](classOf[java.lang.String])): ClassTag[String])`
        val app = getParameter(t.expr)
        app match {
          case app: BreakDown =>
            app.copy(targs = Seq(t.tpe), pos = t.pos, code = t.syntax)
          case app: Param => app
        }
      }
      case t: Term.Name => RawCode(t.syntax, t.pos)
      case t: Term.Placeholder => RawCode(t.syntax, t.pos)
      case t: Term.Interpolate => RawCode(t.syntax, t.pos)
      case t: Lit => RawCode(t.syntax, t.pos)
      case t: Term => {
        val bd = breakDown(t)
        bd.symbol.app match {
          case Some(s) => bd
          case None => RawCode(t.syntax, t.pos)
        }
      }
    }
    params
      .filterNot(x => {
        x.toString() == "*"
      })
      .map(getParameter)
  }

  private def breakDown(tree: Term)(
      implicit finder: Tree => QualifiedSymbol): BreakDown = {

    tree match {
      case t: Term.Apply => {
        // Anything with a parameter list (`hello(*)`, `hello[String](*)`, `hello[String](*)(stringConverter)`...)
        val bd = breakDown(t.fun)
        bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
      }
      case t: Term.ApplyType => {
        // An application with type parameters but no parameter list
        // examples: `test.this.JsonWriter[Seq[Student]]`
        val bd = breakDown(t.fun)
        bd.copy(targs = t.targs, pos = t.pos, code = t.syntax)
      }
      case t: Term.ApplyInfix => {
        // Infix function applications (e.g. `list map f1`).
        // We take the operation and the ''implicit'' parameters
        val bd = breakDown(t.op)
        bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
      }
      case t: Term.ApplyUnary => {
        // Unary functions
        val bd = breakDown(t.op)
        bd.copy(args = processParamList(Seq(t.arg)),
                pos = t.pos,
                code = t.syntax)
      }
      case t: Term.Select => {
        // Does not have parameters (otherwise it would be a Term.Apply) or type parameters (Term.ApplyType)
        // examples: `test.this.JsonWriter`
        breakDown(t.name).copy(pos = t.pos, code = t.syntax)
      }
      case t: Term.Name => {
        // Plain name of the symbol we want (e.g. in `test.this.JsonWriter` -> `"JsonWriter"`)
        val app = finder(t)
        BreakDown(app, Seq(), Seq(), t.pos)
      }
      case t: Term.New => {
        // Class constructor calls.
        // If the constructor is anonymous, we return the symbol of the class.
        // FIXME: This assumes that we won't find this in a synthetic
        val app = t.init.name match {
          case n: Name.Anonymous => finder(t.init.tpe)
        }
        BreakDown(app, Seq(), Seq(), pos = t.pos, code = t.syntax)
      }
      case t: Term.NewAnonymous => {
        // Anonymously constructed objects:
        // `new SnapshotFixtures { val buffer = // ... }`
        // We assume we have only one Init
        assert(t.templ.inits.size == 1,
               s"More than one init found for NewAnonymous ${t}")
        InitDecomposer(t.templ.inits.head, finder)
      }
      case t: Term.Function => {
        // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
        // We assume that the body is a single function call, as is the most typical in passing parameters
        // We also ignore the parameters for now, since they will appear in other call sites,
        // when the function gets executed
        breakDown(t.body)
      }
      case t: Term.Interpolate => {
        val bd = breakDown(t.prefix)
        bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
      }
      case t => {
        println(t.structure)
        throw new MatchError(s"Unknown match ${t}")
      }
    }
  }
}

object InitDecomposer {
  def apply(init: Init, finder: Tree => QualifiedSymbol): BreakDown =
    BreakDown(
      symbol = finder(init.name),
      targs = Seq(init.tpe), //TODO not entirely sure about this, it will appear like [fun[Targs]]
      args = TermDecomposer.processParamList(init.argss.flatten)(finder),
      pos = init.pos
    )
}

object Queries {

  def breakDownSynthetic(ctx: SemanticCtx,
                         synth: Synthetic): SyntheticBreakdown = {

    def parse(text: String): Term = text.parse[Term].get

    def finder(tree: Tree): QualifiedSymbol = {
      synth.names.find(_.position.end == tree.pos.end) match {
        // Filter out the _star_ names
        // TODO: I don't think this case is relevant anymore, see processParamList()
        case Some(n) if n.symbol.syntax.contains("_star_") =>
          QualifiedSymbol.Empty
        case Some(name) =>
          QualifiedSymbol(Some(name.symbol), isSynthetic = true)
        case None => QualifiedSymbol.Empty
      }
    }

    val processedSynthetic = {
      val bd = TermDecomposer(parse(synth.text), finder).copy(
        pos = synth.position
      )
      SyntheticBreakdown(
        breakDown = bd,
        SyntheticOrigins(
          application = if (bd.symbol.app.isDefined) Some(synth) else None,
          paramList = Some(synth)
        )
      )
    }
    val res = processedSynthetic.breakDown.symbol.app match {
      case Some(app) => processedSynthetic
      case None => {
        val matchedApplication = findApplication(ctx, synth)
        //assertWeCanEraseParams(matchedApplication)
        matchedApplication.copy(
          matchedApplication.breakDown.copy(
            args = processedSynthetic.breakDown.args
          ),
          matchedApplication.origins.copy(
            paramList = processedSynthetic.origins.paramList
          )
        )
      }
    }
    assert(
      res.breakDown.symbol.app.isDefined,
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
  def findApplication(ctx: SemanticCtx, synth: Synthetic): SyntheticBreakdown = {

    def breakdownTree(term: Tree): BreakDown = {
      def finder(t: Tree): QualifiedSymbol = {
        val sym = ctx.symbol(t)
        // A symbol from the tree will never be synthetic
        t match {
          // Special case: https://github.com/PRL-PRG/scalafix-rule-workshop/issues/39
          case tree: Term.Name
              if sym.isDefined &&
                sym.get.isInstanceOf[Symbol.Local] =>
            QualifiedSymbol(ctx.unrecurse(tree), isSynthetic = false)
          case tree =>
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
        case t: Init => InitDecomposer(term.asInstanceOf[Init], finder)
        case t: Term => TermDecomposer(term.asInstanceOf[Term], finder)
      }
    }

    ctx.syntheticApplication(synth.position.end) match {
      // There is a synthetic application that matches
      case Some(syntheticApply) => breakDownSynthetic(ctx, syntheticApply)
      // Parse from the tree itself
      case None =>
        SyntheticBreakdown(
          breakdownTree(ctx
            .inSourceCallSite(synth.position.end)
            .getOrElse {
              throw new RuntimeException(
                s"No application found in source for ${synth.text}@${synth.position.endLine}:${synth.position.endColumn}")
            })
        )
    }
  }

}

case class ExtractionResult(callSites: Seq[CallSite],
                            declarations: Set[Declaration])
object ExtractionResult {
  val Empty = ExtractionResult(Seq(), Set())
}
object ReflectExtract extends (ReflectiveCtx => ExtractionResult) {

  def extractCallSites(ctx: ReflectiveCtx) =
    ctx.syntheticsWithImplicits
      .map(Queries.breakDownSynthetic(ctx, _))
      .map(ctx.reflectOnBreakdown)
      .map(Factories.createCallSite(ctx, _))

  def extractDeclarations(ctx: ReflectiveCtx) =
    ctx.inSourceDefinitions
      .filter(_._3.isImplicit)
      .map(x => (x._1, ctx.findReflectSymbol(x._2), x._3))
      .map(x => DeclarationReflection(ctx, x._1, x._2, Some(x._3)))
      .map(Factories.createDeclaration(ctx, _))
      .toSet

  def apply(ctx: ReflectiveCtx): ExtractionResult = {
    val callSites = extractCallSites(ctx)

    val declarations: Set[Declaration] = extractDeclarations(ctx)

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

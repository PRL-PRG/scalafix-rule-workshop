package cz.cvut.fit.prl.scalaimplicit.extractor

import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.Representation.TopLevelElem
import cz.cvut.fit.prl.scalaimplicit.extractor.Serializables.{
  Apply,
  DeclaredImplicit,
  FunApplyWithImplicitParam,
  ImplicitParam
}
import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
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

object Queries {
  def syntheticsWithImplicits(ctx: SemanticCtx): Seq[Synthetic] =
    ctx.index.synthetics.filter(_.text.contains("("))

  case class TArg(symbol: Symbol, args: Seq[TArg])
  case class SyntheticBreakdown(app: Option[Symbol],
                                typeParams: Seq[TArg],
                                params: Seq[SyntheticBreakdown])
  def breakdownSynthetic(synth: Synthetic): SyntheticBreakdown = {

    /**
      * We define plain name as everything in the text of a synthetic between the last dot (or hashtag)
      * and the first opening bracket, paren or the end of the string.
      * For example, the plain names of the following are:
      *  - "*.withFilter[A, Iterable[A]\](*)(some.package.B)" => withFilter
      *  - "test.this.JsonWriter(*)" => JsonWriter
      *  - "*.apply" => apply
      * We strip the star prefix so that parameter list names ("*(param.One,param.Two)") will be ""
      */
    def plainName(text: String): String =
      text
        .stripPrefix("*")
        .split("""[\[\(]""")(0)
        .split("""[#\.]""")
        .last

    def parse(text: String): Term = {
      text.parse[Term].get
    }

    def findSymbolAt(pos: Int): Option[Symbol] = {
      synth.names.find(_.position.end == pos) match {
        case Some(name) => Some(name.symbol)
        case None => None
      }
    }

    def processType(tree: Type): TArg = {
      tree match {
        case t: Type.Apply => {
          val pt = processType(t.tpe)
          val targs = t.args.map(processType)
          TArg(pt.symbol, targs)
        }
        case t: Type.Name => {
          val symbol = findSymbolAt(t.pos.end).get
          TArg(symbol, Seq())
        }
      }
    }

    def breakDown(tree: Term): SyntheticBreakdown = {
      tree match {
        case t: Term.Apply => {
          // Anything with a parameter list (`hello(*)`, `hello[String](*)`, `hello[String](*)(stringConverter)`...)
          val bd = breakDown(t.fun)
          bd.copy(params = t.args.map(breakDown))
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
          val app = findSymbolAt(t.pos.end)
          SyntheticBreakdown(app, Seq(), Seq())
        }
        case t: Term.Block => {
          // A block inside the synthetic (e.g. `nested.this.a2c(*)({((a: A) => nested.this.a2b(a))})`)
          // We assume it has only one stat inside, and that it is a Term.
          assert(t.stats.size == 1,
                 s"Body ${t.stats} of block $t has more than one stat")
          assert(t.stats.forall(_.isInstanceOf[Term]),
                 s"Stat (${t.stats.head}) from block ${t} is not a term")
          // FIXME this is a bit of a hack, putting all the statements as parameters of a None-callsite
          breakDown(t.stats.head.asInstanceOf[Term])
        }
        case t: Term.Function => {
          // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
          // We assume that the body is a single function call, as is the most typical in passing parameters
          // We also ignore the parameters for now, since they will appear in other call sites,
          // when the function gets executed
          breakDown(t.body)
        }
      }
    }

    val tree = parse(synth.text)
    breakDown(tree)
  }

  /*
  /**
   * We find applications for the synthetics that don't have them (that is, pure parameter lists)
   * For that, we try to look for a fitting `apply` synthetic. If we don't find one, we look in
   * the source code and try to match there.
   *
   * In both cases, we match by position, since parameter lists are inserted at the end of calls.
   *
   * We assume that there is exactly one symbol at the position of the synthetic.
   */
  def matchWithInSourceApplications(
      ctx: SemanticCtx,
      breakdowns: Seq[SyntheticBreakdown]): Seq[SyntheticBreakdown] = {

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
   *
   * ''Note that'' it is lazy, since this is a computationally expensive operation, and there will be
   * cases where it is not needed (e.g. if all breakdowns have apps defined)
   */
    lazy val inSourceCallSites: Map[Int, Option[Symbol]] =
      (ctx.tree collect {
        case x: Term.Apply => x.fun.pos.end -> ctx.symbol(x)
        case x: Term.Name  => x.pos.end -> ctx.symbol(x)
      }).toMap

    /**
   * Filter for the synthetic function applications.
   * Captures all "apply()" functions inserted by the compiler.
   * @param elem Synthetic to test
   * @return true iff the synthetic has apply in the name
   */
    def hasApplyInTheName(elem: Synthetic): Boolean = {
      elem.names.exists(_.toString() == "apply")
    }
    def getApply(elem: Synthetic): (Int, Symbol) = {
      elem.position.end -> elem.names.find(_.toString == "apply").get.symbol
    }
    val syntheticApplies: Map[Int, Symbol] =
      ctx.index.synthetics.filter(hasApplyInTheName).map(getApply).toMap

    breakdowns.collect {
      case bd if bd.app.isDefined => bd
      case bd =>
        syntheticApplies.find(_._1 == bd.pos) match {
          case Some(callSite) => bd.copy(app = Some(callSite._2))
          case None =>
            bd.copy(
              app =
                inSourceCallSites.find(_._1 == bd.pos).getOrElse(0 -> None)._2)
        }
    }
  }
   */

  import scala.reflect.runtime.{universe => u}
  case class ReflectiveBreakdown(app: Option[Set[u.Symbol]],
                                 params: Seq[Set[u.Symbol]],
                                 typeParams: Seq[Set[u.Symbol]])

  /**
    * Query the context to fetch the reflective symbols of every relevant
    * scala.meta symbol in the synthetic
    * @param ctx
    * @param breakdown
    * @return
    */
  def getReflectiveSymbols(
      ctx: ReflectiveCtx,
      breakdown: SyntheticBreakdown): ReflectiveBreakdown = {
    ReflectiveBreakdown(
      app = breakdown.app match {
        case Some(a) => Some(ctx.fetchReflectSymbol(a))
        case _ => None
      },
      params = breakdown.params.map(x => ctx.fetchReflectSymbol(x.app.get)),
      typeParams =
        breakdown.typeParams.map(x => ctx.fetchReflectSymbol(x.symbol))
    )
  }
}

object ReflectExtract extends (ReflectiveCtx => Seq[TopLevelElem]) {
  def apply(ctx: ReflectiveCtx): Seq[TopLevelElem] = {
    val implicits = Queries.syntheticsWithImplicits(ctx)
    val breakDowns = implicits.map(s => (s, Queries.breakdownSynthetic(s)))
    /*val matched = Queries.matchWithInSourceApplications(ctx, breakDowns)
    assert(
      !matched.exists(_.app.isEmpty),
      s"Some synthetic(s) didn't find an application: ${matched.filter(_.app.isEmpty).mkString("\n")}")

    val reflectSymbols = breakDowns.map(Queries.getReflectiveSymbols(ctx, _))
     */
    breakDowns.map(x => println(x._2))
    Seq()
  }
}

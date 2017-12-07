package cz.cvut.fit.prl.scalaimplicit.extractor

import cz.cvut.fit.prl.scalaimplicit.extractor.Representation.TopLevelElem
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
      case Input.File(path, _)        => path.toString
      case _                          => ""
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

  case class SyntheticBreakdown(synthetic: Synthetic,
                                app: Option[ResolvedName],
                                params: Seq[ResolvedName],
                                typeParams: Seq[ResolvedName])
  def breakdownSynthetic(synth: Synthetic): SyntheticBreakdown = {

    /**
      * Returns true if a given resolved name is a resolved type.
      * The discriminator is that type symbols end in hashtag (#),
      * as shown here: https://goo.gl/Z6mXdN
      * '''Note that not all type parameters belong to the application. e.g.:'''
      *  "test.this.JsonWriter[Seq[Student]](*)(test.this.seq2json[Student](test.this.Student2Json))"
      *  will have 3 type params, "Seq" and "Student" for the application,
      *  and an extra "Student" for seq2json
      * @param rn
      * @return
      */
    def isType(rn: ResolvedName): Boolean = rn.symbol.syntax.endsWith("#")

    /**
      * The opposite of isType, a term or method ends with a dot.
      * Refer to the link in isType for proof.
      * @param rn
      * @return
      */
    def isTermOrMethod(rn: ResolvedName): Boolean =
      rn.symbol.syntax.endsWith(".") && !rn.symbol.syntax.startsWith("_star_")

    /**
      * We define plain name as everything in the text of a synthetic between the last dot (or hashtag)
      * and the first opening bracket, paren or the end of the string.
      * For example, the plain names of the following are:
      *  - "*.withFilter[A, Iterable[A]\](*)(some.package.B)" => withFilter
      *  - "test.this.JsonWriter(*)" => JsonWriter
      *  - "*.apply" => apply
      * We strip the star prefix so that parameter list names ("*(param.One,param.Two)") will be ""
      */
    val plainName: String =
      synth.text.stripPrefix("*").split("""[\[\(]""")(0).split("""[#\.]""").last

    /**
      * A conversion is a method that has the same plain name as the synthetic it served.
      * This method serves to identify which name of an implicit conversion synthetic
      * is the method being called.
      * It's symbol will have the form "_root_.to.package.Conversion(L/param)L/ret", and
      * productElement(1) corresponds to the jvmSignature (according to this: https://goo.gl/Z6mXdN).
      * In this case, for example, productElement(1) would be "Conversion(L/param)L/ret", and
      * "Conversion" should coincide with the plainName
      * @param rn
      * @return
      */
    def isConversion(rn: ResolvedName): Boolean =
      isTermOrMethod(rn) && rn.symbol
        .productElement(1)
        .toString
        .startsWith(plainName)

    /**
      * Every term or method that is not a conversion in a synthetic
      * is part of the implicit parameter list. The rest are type parameters.
      * '''Note that not all parameters belong to the application. e.g.:'''
      *  "test.this.JsonWriter[Seq[Student]](*)(test.this.seq2json[Student](test.this.Student2Json))"
      *  will have 2 params, "seq2json" and "Student2Json"
      * @param rn
      * @return
      */
    def isParameter(rn: ResolvedName): Boolean =
      isTermOrMethod(rn) && !isConversion(rn)

    SyntheticBreakdown(
      synthetic = synth,
      app = synth.names.find(isConversion),
      params = synth.names.filter(isParameter),
      typeParams = synth.names.filter(isType)
    )
  }

  import scala.reflect.runtime.{universe => u}
  case class ReflectiveBreakdown(synthetic: Synthetic,
                                 app: Option[u.Symbol],
                                 params: Seq[u.Symbol],
                                 typeParams: Seq[u.Symbol])

  /**
    * Query the context to fetch the reflective symbols of every relevant
    * resolved name in the synthetic
    * @param ctx
    * @param breakdown
    * @return
    */
  def getReflectiveSymbols(
      ctx: ReflectiveCtx,
      breakdown: SyntheticBreakdown): ReflectiveBreakdown = {
    ReflectiveBreakdown(
      synthetic = breakdown.synthetic,
      app = breakdown.app match {
        case Some(a) => Some(ctx.fetchReflectSymbol(a.symbol))
        case _       => None
      },
      params = breakdown.params.map(x => ctx.fetchReflectSymbol(x.symbol)),
      typeParams =
        breakdown.typeParams.map(x => ctx.fetchReflectSymbol(x.symbol))
    )
  }

}

object ReflectExtract extends (ReflectiveCtx => Seq[TopLevelElem]) {
  def apply(ctx: ReflectiveCtx): Seq[TopLevelElem] = {
    val implicits = Queries.syntheticsWithImplicits(ctx)
    val breakDowns = implicits.map(Queries.breakdownSynthetic)
    val syntheticApplications = breakDowns.filter(_.app.isDefined)
    val declarations =
      syntheticApplications.map(x => ctx.signature(x.app.get.symbol))
    val reflectSymbols = breakDowns.map(Queries.getReflectiveSymbols(ctx, _))
    Seq()
  }
}

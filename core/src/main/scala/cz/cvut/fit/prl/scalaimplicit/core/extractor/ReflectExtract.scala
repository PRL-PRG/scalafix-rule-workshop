package cz.cvut.fit.prl.scalaimplicit.core.extractor

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  CallSite,
  Declaration
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.decomposers.{
  DefnDecomposer,
  InitDecomposer,
  TermDecomposer
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Factories

import scala.meta._
import scala.reflect.runtime.{universe => u}
import scala.util.{Failure, Success, Try}

/**
  * A collection of methods that drive the extraction of call sites and declarations
  */
object CallSiteExtractionUtils {
  def hasImplicits(ctx: ReflectiveCtx, breakdown: DefnBreakdown): Boolean = {
    def hasImplicitsRef(ref: u.Symbol): Boolean = {
      ref.isImplicit || ref.typeSignature.paramLists.exists(ls =>
        ls.exists(param => hasImplicitsRef(param)))
    }

    def hasImplicitsDen(den: Option[Denotation]): Boolean = {
      den match {
        case Some(d) =>
          d.isImplicit || d.names.exists(n =>
            hasImplicitsDen(ctx.denotation(n.symbol)))
        case None => false
      }
    }

    hasImplicitsDen(breakdown.den) || (breakdown.sym.isDefined && hasImplicitsRef(
      breakdown.sym.get))
  }

  def breakDownSynthetic(ctx: SemanticCtx,
                         synth: Synthetic): CallSiteBreakDown = {

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
      CallSiteBreakDown(
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
  def findApplication(ctx: SemanticCtx, synth: Synthetic): CallSiteBreakDown = {

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
                sym.getOrElse(
                  Symbol(ctx.qualifiedName(t.asInstanceOf[Term])))),
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
        CallSiteBreakDown(
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

object DefnExtractionUtils {
  def getDefn(ctx: ReflectiveCtx, tree: Tree): Seq[DefnBreakdown] = {
    def finder(t: Tree): Symbol = ctx.symbol(t).get
    DefnDecomposer(ctx, tree)(finder)
  }
}

case class ImplicitAnalysisResult(callSites: Seq[CallSite],
                                  declarations: Set[Declaration])
object ImplicitAnalysisResult {
  def merge(one: ImplicitAnalysisResult,
            other: ImplicitAnalysisResult): ImplicitAnalysisResult =
    ImplicitAnalysisResult(
      one.callSites ++ other.callSites,
      one.declarations ++ other.declarations
    )

  val Empty = ImplicitAnalysisResult(Seq(), Set())
}

/**
  * Extract the call sites from a context, starting with the synthetics.
  */
object ReflectExtract extends (ReflectiveCtx => ImplicitAnalysisResult) {

  def extractCallSites(ctx: ReflectiveCtx): Seq[CallSite] =
    ctx.syntheticsWithImplicits
      .map(
        syn =>
          Try(
            Factories.createCallSite(
              ctx,
              ctx.reflectOnCallSite(
                CallSiteExtractionUtils.breakDownSynthetic(ctx, syn)
              ))
        )
      )
      .reportAndExtract("CallSite")

  def extractDeclarations(ctx: ReflectiveCtx): Set[Declaration] =
    ctx.inSourceDefinitions
      .map(
        t =>
          Try(
            DefnExtractionUtils
              .getDefn(ctx, t)
        )
      )
      .reportAndExtract("Definition")
      .flatten
      .map(d =>
        Factories.createDeclaration(ctx, DeclarationReflection(ctx, d)))
      .toSet

  // Helper class for filtering erroneous results the extraction.
  // Returns the same collection, only keeping the Successes
  // and reporting the Failures.
  private implicit class TryCollection[A](from: Seq[Try[A]]) {
    def reportAndExtract(header: String): Seq[A] =
      from
        .map {
          case Failure(t) => {
            ErrorCollection().report(header, t)
          }; Failure(t)
        case t => t
        }
        .collect {
          case Success(t) => t
        }
  }

  def apply(ctx: ReflectiveCtx): ImplicitAnalysisResult = {
    ImplicitAnalysisResult(extractCallSites(ctx), extractDeclarations(ctx))
  }
}

/**
  * Version of extraction that stops once it finds an exception
  */
object FailFastReflectExtract
    extends (ReflectiveCtx => ImplicitAnalysisResult) {
  // Replaced with instance-by-instance processing to be able to log exceptions easily
  def failFastExtractCallSites(ctx: ReflectiveCtx) =
    ctx.syntheticsWithImplicits
      .map(CallSiteExtractionUtils.breakDownSynthetic(ctx, _))
      .map(ctx.reflectOnCallSite)
      .map(Factories.createCallSite(ctx, _))

  def failFastExtractDeclarations(ctx: ReflectiveCtx) =
    ctx.inSourceDefinitions
      .flatMap(DefnExtractionUtils.getDefn(ctx, _))
      //.filter(Queries.hasImplicits(ctx, _))
      .map(DeclarationReflection(ctx, _))
      .map(Factories.createDeclaration(ctx, _))
      .toSet

  def apply(ctx: ReflectiveCtx): ImplicitAnalysisResult = {
    ImplicitAnalysisResult(failFastExtractCallSites(ctx),
                           failFastExtractDeclarations(ctx))
  }
}

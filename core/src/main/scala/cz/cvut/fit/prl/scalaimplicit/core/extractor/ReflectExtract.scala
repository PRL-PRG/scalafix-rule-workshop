package cz.cvut.fit.prl.scalaimplicit.core.extractor

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts._
import cz.cvut.fit.prl.scalaimplicit.schema._

/**
  * A collection of methods that drive the extraction of call sites and declarations
  */
//object CallSiteExtractionUtils {
//  def hasImplicits(ctx: ReflectiveCtx, breakdown: DefnBreakdown): Boolean = {
//    def hasImplicitsRef(ref: u.Symbol): Boolean = {
//      ref.isImplicit || ref.typeSignature.paramLists.exists(ls =>
//        ls.exists(param => hasImplicitsRef(param)))
//    }
//
//    def hasImplicitsDen(den: Option[Denotation]): Boolean = {
//      den match {
//        case Some(d) =>
//          d.isImplicit || d.names.exists(n =>
//            hasImplicitsDen(ctx.denotation(n.symbol)))
//        case None => false
//      }
//    }
//
//    hasImplicitsDen(breakdown.den) || (breakdown.sym.isDefined && hasImplicitsRef(
//      breakdown.sym.get))
//  }
//
//
//
//
//}

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

  //  def extractCallSites(ctx: ReflectiveCtx): Seq[CallSite] =
  //    ctx.syntheticsWithImplicits
  //      .map(
  //        syn =>
  //          Try(
  //            Factories.createCallSite(
  //              ctx,
  //              ctx.reflectOnCallSite(
  //                CallSiteExtractionUtils.breakDownSynthetic(ctx, syn)
  //              ))
  //        )
  //      )
  //      .reportAndExtract("CallSite")
  //
  //  def extractDeclarations(ctx: ReflectiveCtx): Set[Declaration] =
  //    ctx.inSourceDefinitions
  //      .map(
  //        t =>
  //          Try(
  //            DefnExtractionUtils
  //              .getDefn(ctx, t)
  //        )
  //      )
  //      .reportAndExtract("Definition")
  //      .flatten
  //      .map(d =>
  //        Factories.createDeclaration(ctx, DeclarationReflection(ctx, d)))
  //      .toSet
  //
  //  // Helper class for filtering erroneous results the extraction.
  //  // Returns the same collection, only keeping the Successes
  //  // and reporting the Failures.
  //  private implicit class TryCollection[A](from: Seq[Try[A]]) {
  //    def reportAndExtract(header: String): Seq[A] =
  //      from
  //        .map {
  //          case Failure(t) => {
  //            ErrorCollection().report(header, t)
  //          }; Failure(t)
  //        case t => t
  //        }
  //        .collect {
  //          case Success(t) => t
  //        }
  //  }

  def apply(ctx: ReflectiveCtx): ImplicitAnalysisResult = {
    //    ImplicitAnalysisResult(extractCallSites(ctx), extractDeclarations(ctx))
    ImplicitAnalysisResult(null, null)
  }
}

/**
  * Version of extraction that stops once it finds an exception
  */
// FIXME: remove
object FailFastReflectExtract
    extends (ReflectiveCtx => ImplicitAnalysisResult) {

  def apply(ctx: ReflectiveCtx): ImplicitAnalysisResult =
    ctx.result
}

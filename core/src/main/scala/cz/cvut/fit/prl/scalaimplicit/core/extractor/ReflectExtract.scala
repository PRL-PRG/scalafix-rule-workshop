package cz.cvut.fit.prl.scalaimplicit.core.extractor

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts._
import cz.cvut.fit.prl.scalaimplicit.schema._

import scala.util.{Failure, Success, Try}

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
  def apply(ctx: ReflectiveCtx): ImplicitAnalysisResult = ctx.analyze
}

/**
  * Version of extraction that stops once it finds an exception
  */
// FIXME: remove
object FailFastReflectExtract extends (ReflectiveCtx => ImplicitAnalysisResult) {
  def apply(ctx: ReflectiveCtx): ImplicitAnalysisResult = ctx.analyzeChecked
}

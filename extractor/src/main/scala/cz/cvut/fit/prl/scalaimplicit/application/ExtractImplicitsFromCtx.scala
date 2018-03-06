package cz.cvut.fit.prl.scalaimplicit.application

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ImplicitAnalysisResult,
  ReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import cz.cvut.fit.prl.scalaimplicit.core.runners.ReflectiveContextProcessing

object ExtractImplicitsFromCtx
    extends ReflectiveContextProcessing[ImplicitAnalysisResult] {
  override def processCtx(ctx: ReflectiveCtx): ImplicitAnalysisResult =
    ReflectExtract(ctx)

  override def createEmpty: ImplicitAnalysisResult =
    ImplicitAnalysisResult.Empty

  override def merge(one: ImplicitAnalysisResult,
                     other: ImplicitAnalysisResult): ImplicitAnalysisResult =
    ImplicitAnalysisResult.merge(one, other)
}

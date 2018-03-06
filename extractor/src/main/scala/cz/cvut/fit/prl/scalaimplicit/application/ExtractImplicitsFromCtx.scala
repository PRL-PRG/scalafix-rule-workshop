package cz.cvut.fit.prl.scalaimplicit.application

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ImplicitAnalysisResult, ReflectExtract}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import cz.cvut.fit.prl.scalaimplicit.core.runners.ReflectiveContextProcessing

object ExtractImplicitsFromCtx
    extends ReflectiveContextProcessing[ImplicitAnalysisResult] with LazyLogging {
  override def processCtx(ctx: ReflectiveCtx): ImplicitAnalysisResult = {
    logger.debug(s"Exracting implicits from ${ctx.file}")
    ReflectExtract(ctx)
  }

  override def createEmpty: ImplicitAnalysisResult =
    ImplicitAnalysisResult.Empty

  override def merge(one: ImplicitAnalysisResult,
                     other: ImplicitAnalysisResult): ImplicitAnalysisResult =
    ImplicitAnalysisResult.merge(one, other)
}

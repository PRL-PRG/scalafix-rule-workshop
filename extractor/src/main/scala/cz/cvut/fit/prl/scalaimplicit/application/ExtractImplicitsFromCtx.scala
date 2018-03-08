package cz.cvut.fit.prl.scalaimplicit.application

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ImplicitAnalysisResult,
  ReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import cz.cvut.fit.prl.scalaimplicit.core.runners.SemanticDBProcessing
import org.langmeta.semanticdb.Database

class ExtractImplicitsFromCtx(loader: ClassLoader)
    extends SemanticDBProcessing[ImplicitAnalysisResult]
    with LazyLogging {
  override def processDB(db: Database): ImplicitAnalysisResult = {
    val ctx = new ReflectiveCtx(loader, db)
    logger.debug(s"Exracting implicits from ${ctx.file}")
    ReflectExtract(ctx)
  }

  override def createEmpty: ImplicitAnalysisResult =
    ImplicitAnalysisResult.Empty

  override def merge(one: ImplicitAnalysisResult,
                     other: ImplicitAnalysisResult): ImplicitAnalysisResult =
    ImplicitAnalysisResult.merge(one, other)
}
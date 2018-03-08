package cz.cvut.fit.prl.scalaimplicit.core.runners

import java.nio.file.Files

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ErrorCollection,
  ImplicitAnalysisResult,
  OrphanCallSites,
  ReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  Argument,
  ArgumentLike,
  Declaration,
  ImplicitArgument
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
}
import org.langmeta.internal.io.PathIO
import org.langmeta.semanticdb.Database

import scala.meta.AbsolutePath

trait SemanticDBProcessing[A] {
  def processDB(db: Database): A
  def createEmpty: A
  def merge(one: A, other: A): A
}

object TreeWalker extends LazyLogging {
  def apply[A](rootPath: String, processing: SemanticDBProcessing[A]): A = {

    val root = AbsolutePath(rootPath)
    logger.debug(s"Analyzing ${rootPath}")
    import scala.collection.JavaConverters.asScalaIteratorConverter
    //TODO MAKE A PROPER CLASS HIERARCHY
    //deleteOldFiles(root)
    Files
      .walk(root.toNIO)
      .iterator()
      .asScala
      .filter { file =>
        Files.isRegularFile(file) &&
        PathIO.extension(file) == "semanticdb"
      }
      .toSeq
      .par
      .map(file => {
        logger.debug(s"Processing ${file}")
        processing.processDB(DBOps.loadDB(file))
      })
      .fold(processing.createEmpty)(processing.merge)
  }
}

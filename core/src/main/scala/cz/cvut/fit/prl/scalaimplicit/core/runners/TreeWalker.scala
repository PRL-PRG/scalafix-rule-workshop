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

import scala.meta.AbsolutePath

trait ReflectiveContextProcessing[A] {
  def processCtx(ctx: ReflectiveCtx): A
  def createEmpty: A
  def merge(one: A, other: A): A
}

class TreeWalker(loader: ClassLoader, rootPath: String) extends LazyLogging {
  val root = AbsolutePath(rootPath)
  logger.debug(s"Analyzing ${rootPath}")
  def apply[A](extractable: ReflectiveContextProcessing[A]): A = {
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
      .map(file => new ReflectiveCtx(loader, DBOps.loadDB(file)))
      .map(extractable.processCtx)
      .fold(extractable.createEmpty)(extractable.merge)
  }
}


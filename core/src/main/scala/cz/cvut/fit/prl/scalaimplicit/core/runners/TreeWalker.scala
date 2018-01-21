package cz.cvut.fit.prl.scalaimplicit.core.runners

import java.nio.file.Files

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
}
import org.langmeta.internal.io.PathIO

import scala.meta.AbsolutePath

class TreeWalker(loader: ClassLoader, rootPath: String) extends LazyLogging {
  val root = AbsolutePath(rootPath)
  logger.debug(s"Analyzing ${rootPath}")
  def apply(f: ReflectiveCtx => ExtractionResult): ExtractionResult = {
    import scala.collection.JavaConverters.asScalaIteratorConverter
    //TODO MAKE A PROPER CLASS HIERARCHY
    //deleteOldFiles(root)
    val results = Files
      .walk(root.toNIO)
      .iterator()
      .asScala
      .filter { file =>
        Files.isRegularFile(file) &&
        PathIO.extension(file) == "semanticdb"
      }
      .toSeq
      .par
      .map { file =>
        ReflectiveVisitor(file, loader, f)
      }
      .fold(ExtractionResult.Empty)(
        (acc, res) =>
          ExtractionResult(
            callSites = acc.callSites ++ res.callSites,
            declarations = acc.declarations ++ res.declarations
        ))
    results
  }
}

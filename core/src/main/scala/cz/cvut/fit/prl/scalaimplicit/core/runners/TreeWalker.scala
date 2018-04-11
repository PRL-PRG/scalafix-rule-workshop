package cz.cvut.fit.prl.scalaimplicit.core.runners

import java.nio.file.Files

import com.typesafe.scalalogging.LazyLogging
import org.langmeta.internal.io.PathIO
import org.langmeta.semanticdb.Database

import scala.meta.AbsolutePath

trait SemanticDBProcessing[A] {
  def processDB(db: Database): A
  def createEmpty: A
  def merge(one: A, other: A): A

  def empty(): A
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
        try {
          processing.processDB(DBOps.loadDB(file))
        } catch {
          case e =>
            logger.warn(s"Unable to process ${file}", e)
            processing.empty()
        }
      })
      .fold(processing.createEmpty)(processing.merge)
  }
}

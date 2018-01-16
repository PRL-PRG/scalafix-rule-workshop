package cz.cvut.fit.prl.scalaimplicit.core.extractor.runners

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ExtractionResult, Result}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SemanticCtx
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.{semanticdb => d}

import scala.util.control.NonFatal

object DBOps {
  // Transform the database from the internal format to the scala.meta format
  private def toMetaDB(from: s.Database): d.Database = {
    // Discard documents that have only warning messages
    def discardMessageDocs(docs: Seq[s.Document]): Seq[s.Document] =
      docs.filterNot(doc => doc.messages.nonEmpty && doc.contents.isEmpty)

    from.copy(documents = discardMessageDocs(from.documents)).toDb(None)
  }

  def loadDB(filePath: Path): d.Database = {
    toMetaDB(s.Database.parseFrom(Files.readAllBytes(filePath)))
  }
}

object SemanticDBFileVisitor
    extends ((Path, (SemanticCtx => Result)) => Result)
    with LazyLogging {
  def apply(filePath: Path, f: SemanticCtx => Result): Result = {
    try {
      val mdb = DBOps.loadDB(filePath)
      val ctx = SemanticCtx(mdb)
      val res = f(ctx)
      logger.debug(s"Processing $filePath")
      res
    } catch {
      case NonFatal(e) =>
        val st = e.getStackTrace
        e.setStackTrace(st.take(10))
        e.printStackTrace()
        Result.Empty
    }
  }
}

object ReflectiveVisitor
    extends ((Path, ClassLoader, (ReflectiveCtx => ExtractionResult)) => ExtractionResult)
    with LazyLogging {
  def apply(filePath: Path,
            loader: ClassLoader,
            f: ReflectiveCtx => ExtractionResult): ExtractionResult = {
    try {
      val mdb = DBOps.loadDB(filePath)
      val ctx = new ReflectiveCtx(loader, mdb)
      logger.debug(s"Processing $filePath")
      val res = f(ctx)
      res
    } catch {
      case NonFatal(e) =>
        val st = e.getStackTrace
        e.setStackTrace(st.take(10))
        e.printStackTrace()
        ExtractionResult.Empty
    }
  }
}

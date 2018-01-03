package cz.cvut.fit.prl.scalaimplicit.core.extractor.runners

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ExtractionResult, Result}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SemanticCtx
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import org.langmeta.internal.semanticdb.{schema => s}

import scala.util.control.NonFatal

object SemanticDBFileVisitor
    extends ((Path, (SemanticCtx => Result)) => Result)
    with LazyLogging {
  def apply(filePath: Path, f: SemanticCtx => Result): Result = {
    try {
      val sdb = s.Database.parseFrom(Files.readAllBytes(filePath))
      val mdb = sdb.toDb(None)
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
      val sdb = s.Database.parseFrom(Files.readAllBytes(filePath))
      val mdb = sdb.toDb(None)
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

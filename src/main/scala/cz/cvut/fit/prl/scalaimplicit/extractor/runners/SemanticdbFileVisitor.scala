package cz.cvut.fit.prl.scalaimplicit.extractor.runners

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.extractor.Result
import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.SemanticCtx
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

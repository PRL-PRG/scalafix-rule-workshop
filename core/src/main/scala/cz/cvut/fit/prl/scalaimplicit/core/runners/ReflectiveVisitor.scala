package cz.cvut.fit.prl.scalaimplicit.core.runners

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  OrphanCallSites,
  ExtractionResult
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SemanticCtx
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation._
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.{semanticdb => d}

import scala.util.Try
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
      DefnFiller(res)
      res
    } catch {
      case NonFatal(e) =>
        e.printStackTrace()
        ExtractionResult.Empty
    }
  }
}

object DefnFiller extends (ExtractionResult => ExtractionResult) {
  def findDeclOrReport(target: {
    def declaration: Declaration; def name: String
  }, definitions: Set[Declaration]): Declaration =
    definitions
      .find(_.name == target.name)
      .getOrElse({
        OrphanCallSites().report(s"Declaration not found for {$target.name}")
        target.declaration
      })

  def processArgList(args: Seq[ArgumentLike],
                     definitions: Set[Declaration]): Seq[ArgumentLike] = {
    args.map {
      case arg: Argument => arg
      case iarg: ImplicitArgument =>
        iarg.copy(
          declaration = findDeclOrReport(iarg, definitions),
          arguments = processArgList(iarg.arguments, definitions)
        )
    }
  }

  def apply(result: ExtractionResult): ExtractionResult = {
    val defns = result.declarations
    val nres = result.copy(
      declarations = defns,
      callSites = result.callSites.map(
        cs =>
          cs.copy(
            declaration = findDeclOrReport(cs, defns),
            implicitArguments = processArgList(cs.implicitArguments, defns)
        ))
    )
    OrphanCallSites().toFile("./tmp/orphancallsites.log")
    nres
  }
}

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

class TreeWalker(loader: ClassLoader, rootPath: String) extends LazyLogging {
  val root = AbsolutePath(rootPath)
  logger.debug(s"Analyzing ${rootPath}")
  def apply(
      f: ReflectiveCtx => ImplicitAnalysisResult): ImplicitAnalysisResult = {
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
      .map(file => new ReflectiveCtx(loader, DBOps.loadDB(file)))
      .map(ctx => ReflectExtract(ctx))
      .fold(ImplicitAnalysisResult.Empty)(ImplicitAnalysisResult.merge)
    DefnFiller(results)
  }
}

object DefnFiller extends (ImplicitAnalysisResult => ImplicitAnalysisResult) {
  def findDeclOrReport(target: {
    def declaration: Declaration; def name: String
  }, definitions: Set[Declaration]): Declaration =
    definitions
      .find(_.name == target.name)
      .getOrElse({
        OrphanCallSites().report("Orphan CallSite",
                                 s"Declaration not found for {$target.name}")
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

  def apply(result: ImplicitAnalysisResult): ImplicitAnalysisResult = {
    val defns = result.declarations
    val nres = result.copy(
      declarations = defns,
      callSites = result.callSites.map(
        cs =>
          cs.copy(
            declaration = cs.declaration.copy(
              location = findDeclOrReport(cs, defns).location),
            implicitArguments = processArgList(cs.implicitArguments, defns)
        ))
    )
    nres
  }
}

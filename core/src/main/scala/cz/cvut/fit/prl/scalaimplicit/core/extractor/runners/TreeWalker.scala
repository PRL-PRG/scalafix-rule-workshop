package cz.cvut.fit.prl.scalaimplicit.core.extractor.runners

import java.nio.file.Files

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ExtractionResult,
  Result,
  ResultElement
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
}
import org.langmeta.internal.io.PathIO

import scala.meta.AbsolutePath

trait TreeWalker extends ((SemanticCtx => Result) => Result) {
  def apply(f: SemanticCtx => Result): Result
  def deleteOldFiles(projectPath: AbsolutePath): Unit = {
    val files = Files
      .walk(projectPath.toNIO)
      .filter { file =>
        Files.isRegularFile(file) &&
        PathIO.extension(file) == "csv" &&
        file.getFileName.toString != "project.csv"
      }
    files.forEach { file =>
      Files.delete(file)
    }
  }

  /**
    * Function that, given two Results, it returns a Result that contains all the non-duplicate elements of both parameters.
    *
    * @param one
    * @param other
    * @return a Result that contains all the non-duplicate elements of one and other.
    */
  def mergeResults(one: Result, other: Result): Result = {
    def mergeById[A <: ResultElement](one: Set[A], other: Set[A]): Set[A] = {
      (one ++ other)
        .groupBy(_.id)
        .map { case (_, v) => v.head }
        .toSet
    }

    Result(
      mergeById(one.params, other.params),
      one.funs ++ other.funs,
      mergeById(one.links, other.links),
      mergeById(one.implicits, other.implicits)
    )
  }
}

class SingleProjectWalker(rootPath: String)
    extends TreeWalker
    with LazyLogging {
  val root = AbsolutePath(rootPath)
  logger.debug(s"Analyzing ${rootPath}")
  def apply(f: SemanticCtx => Result): Result = {
    import scala.collection.JavaConverters.asScalaIteratorConverter
    deleteOldFiles(root)
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
        SemanticDBFileVisitor(file, f)
      }
      .fold(Result.Empty)(mergeResults)
    results
  }
}

class ReflectiveSingleProjectWalker(loader: ClassLoader, rootPath: String)
    extends LazyLogging {
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

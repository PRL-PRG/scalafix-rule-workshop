package cz.cvut.fit.prl.scalaimplicit.syntheticscounter

import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.runners.{SemanticDBProcessing, TreeWalker}
import org.langmeta.semanticdb.Database

object Main extends LazyLogging {

  def toCSV(res: Set[SyntheticCount]): String = {
    val header = "file,synthetics"
    val values = res.map(_.toCSV).mkString("\n")
    s"${header}\n${values}"
  }

  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        logger.debug(s"Root: ${conf.root}")
        val res = TreeWalker(conf.root, CountSynthetics)
        Files.write(Paths.get(conf.outdir + "/synthetics-counts.csv"),
                    toCSV(res).getBytes)
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

case class SyntheticCount(file: String, synthetics: Int) {
  val toCSV = s"${file},${synthetics}"
}

// These need to be sets in case there are duplicate files.
// Duplicate files can happen as a result of executing sbt assembly, since it bundles
// everything in target.
object CountSynthetics extends SemanticDBProcessing[Set[SyntheticCount]] {
  import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.implicitSemanticDB._

  override def processDB(db: Database): Set[SyntheticCount] = {
    Set(SyntheticCount(db.file, db.synthetics.size))
  }

  override def createEmpty: Set[SyntheticCount] = Set()

  override def merge(one: Set[SyntheticCount],
                     other: Set[SyntheticCount]): Set[SyntheticCount] =
    one ++ other
}

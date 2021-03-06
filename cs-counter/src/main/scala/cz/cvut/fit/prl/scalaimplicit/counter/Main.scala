package cz.cvut.fit.prl.scalaimplicit.counter

import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.runners.{
  SemanticDBProcessing,
  TreeWalker
}
import org.langmeta.ResolvedName
import org.langmeta.semanticdb.{Database, Synthetic}

object Main extends LazyLogging {

  def toCSV(res: Seq[FileCount]): String = {
    val header = "file,insource,synthetic"
    val values = res.map(_.toCSV).mkString("\n")
    s"${header}\n${values}"
  }

  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        logger.debug(s"Root: ${conf.root}")
        val res = TreeWalker(conf.root, CountCallSites)
        Files.write(Paths.get(conf.outdir + "/callsite-counts.csv"),
                    toCSV(res).getBytes)
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

case class FileCount(file: String, syntactic: Int, synthetic: Int) {
  val toCSV = s"${file},${syntactic},${synthetic}"
}

object CountCallSites extends SemanticDBProcessing[Seq[FileCount]] {
  private def hasJVMSignaure(name: ResolvedName): Boolean = {
    name.symbol.syntax.contains("(") && name.symbol.syntax.contains(")")
  }

  import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.implicitSemanticDB._

  def isApply(s: Synthetic): Boolean = s.text.startsWith("*.apply")

  def isConversion(s: Synthetic): Boolean = s.text.contains("(*)")

  override def processDB(db: Database): Seq[FileCount] = {
    val syntactic =
      db.names.count(name => !name.isDefinition && hasJVMSignaure(name))
    val synthetic = db.synthetics.count(s => isConversion(s) || isApply(s))
    Seq(FileCount(db.file, syntactic, synthetic))
  }

  override def createEmpty: Seq[FileCount] = Seq()

  override def merge(one: Seq[FileCount],
                     other: Seq[FileCount]): Seq[FileCount] =
    one ++ other

  override def empty(): Seq[FileCount] = Seq()
}

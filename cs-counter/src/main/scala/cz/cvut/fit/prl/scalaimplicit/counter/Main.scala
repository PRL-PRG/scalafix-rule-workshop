package cz.cvut.fit.prl.scalaimplicit.counter

import java.io.File
import java.net.URLClassLoader
import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.implicitSemanticDB
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ErrorCollection, OrphanCallSites}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import cz.cvut.fit.prl.scalaimplicit.core.runners.{SemanticDBProcessing, TreeWalker}
import org.langmeta.semanticdb.{Database, ResolvedName}

import scala.io.Source

object Main extends LazyLogging {

  def toCSV(res: Seq[CallSiteCount]): String = {
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
        Files.write(Paths.get(conf.outdir + "/callsite-counts.csv"), toCSV(res).getBytes)
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

case class CallSiteCount(file: String, syntactic: Int, synthetic: Int) {
  val toCSV = s"${file},${syntactic},${synthetic}"
}

object CountCallSites extends SemanticDBProcessing[Seq[CallSiteCount]] {

  import implicitSemanticDB._
  private def hasJVMSignaure(name: ResolvedName): Boolean = {
    name.symbol.syntax.contains("(") && name.symbol.syntax.contains(")")
  }

  override def processDB(db: Database): Seq[CallSiteCount] = {
    val syntactic = db.names.count(name => !name.isDefinition && hasJVMSignaure(name))
    val synthetic = db.synthetics.count(_.syntax.contains("(*)"))
    Seq(CallSiteCount(db.file, syntactic, synthetic))
  }

  override def createEmpty: Seq[CallSiteCount] = Seq()

  override def merge(one: Seq[CallSiteCount], other: Seq[CallSiteCount]): Seq[CallSiteCount] = one ++ other
}



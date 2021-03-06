package cz.cvut.fit.prl.scalaimplicit.application

import java.io.{File, FileInputStream}
import java.net.{URL, URLClassLoader}

import com.typesafe.scalalogging.LazyLogging

import cz.cvut.fit.prl.scalaimplicit.core.cli.Cli
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ErrorCollection, OrphanCallSites, ReflectExtract}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.{JSONSerializer, ProtoSerializer}
import cz.cvut.fit.prl.scalaimplicit.core.runners.TreeWalker
import io.circe.generic.auto._

import scala.io.Source

object Main extends LazyLogging {
  def loadClasspath(path: String): String = {
    val src = Source.fromFile(path)(scala.io.Codec("UTF-8"))
    try src.getLines().mkString(":") finally src.close()
  }

  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        logger.debug(s"Root: ${conf.root}")

        val classpath = loadClasspath(conf.classpath)
        val extractFunction = new ExtractImplicitsFromCtx(classpath)
        val res = TreeWalker(conf.root, extractFunction)
        val matchedDefs = DefnFiller(res)

        ProtoSerializer.save(matchedDefs.callSites, conf.outdir + "/results-callsites.proto")
        ProtoSerializer.save(matchedDefs.declarations.toSeq, conf.outdir + "/results-declarations.proto")

        ErrorCollection().toFile(conf.outdir + "/errors.log")
        OrphanCallSites().toFile(conf.outdir + "/orphan-callsites.log")
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

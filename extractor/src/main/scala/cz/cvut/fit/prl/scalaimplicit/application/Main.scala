package cz.cvut.fit.prl.scalaimplicit.application

import java.io.{File, FileInputStream}
import java.net.{URL, URLClassLoader}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.cli.Cli
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ErrorCollection,
  OrphanCallSites,
  ReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import cz.cvut.fit.prl.scalaimplicit.core.runners.TreeWalker

import scala.io.Source

object Main extends LazyLogging {
  def loadClasspath(path: String): ClassLoader = {
    val src = Source.fromFile(path)(io.Codec("UTF-8"))
    val lines = try src.getLines().toArray
    finally src.close()
    new URLClassLoader(lines.map(l => new File(l).toURI.toURL),
                       this.getClass.getClassLoader)
  }

  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        logger.debug(s"Root: ${conf.root}")
        val loader: ClassLoader = loadClasspath(conf.classpath)
        val walker = new TreeWalker(loader, conf.root)
        val res = DefnFiller(walker(ExtractImplicitsFromCtx))
        JSONSerializer.saveJSON(res, conf.outdir + "/results.json")
        ErrorCollection().toFile(conf.outdir + "/errors.log")
        OrphanCallSites().toFile(conf.outdir + "/orphan-callsites.log")
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}



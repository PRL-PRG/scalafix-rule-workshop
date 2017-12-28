package cz.cvut.fit.prl.scalaimplicit.application

import java.io.{File, FileInputStream}
import java.net.{URL, URLClassLoader}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.cli.Cli
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ReflectExtract
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{JSONSerializer}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.runners.ReflectiveSingleProjectWalker

import scala.io.{Codec, Source}

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
        val walker = new ReflectiveSingleProjectWalker(loader, conf.root)
        val res = walker(ReflectExtract)
        JSONSerializer.save(res, conf.root + "/res.dat")
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

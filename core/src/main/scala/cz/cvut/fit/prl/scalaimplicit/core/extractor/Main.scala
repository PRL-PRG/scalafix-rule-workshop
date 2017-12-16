package cz.cvut.fit.prl.scalaimplicit.core.extractor

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.cli.Cli
import cz.cvut.fit.prl.scalaimplicit.core.extractor.runners.SingleProjectWalker

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        logger.debug(s"Root: ${conf.root}")
        val walker = new SingleProjectWalker(conf.root)
        //val results = walker(ExtractImplicits)
        //CSV.dumpFiles(conf.root, results)
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

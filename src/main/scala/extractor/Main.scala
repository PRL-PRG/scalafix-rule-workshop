package extractor

import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Date

import cli._
import com.typesafe.scalalogging.LazyLogging

object Main extends LazyLogging {
  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        logger.debug(s"Root: ${conf.root}")
        val walker = new SingleProjectWalker(conf.root)
        val results = walker(ExtractImplicits)
        CSV.dumpFiles(conf.root, results)
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

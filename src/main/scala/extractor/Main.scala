package extractor

import java.nio.file.{Files, Paths}
import java.text.SimpleDateFormat
import java.util.Date
import cli._

object Main {
  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        println(s"Root: ${conf.root}, Multiple projects: ${conf.multidir}")
        val walker = new SingleProjectWalker(conf.root)
        val results = ExtractImplicits(walker)
        CSV.dumpFiles(conf.root, results)
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

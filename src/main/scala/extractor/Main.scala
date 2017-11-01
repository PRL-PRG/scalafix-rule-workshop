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
        println(s"Root: ${conf.root}\n Multiple projects: ${conf.multidir}")
        val walker = 
          if (conf.multidir) new MultipleProjectWalker(conf.root)
          else new SingleProjectWalker(conf.root)          
        ImplicitParamsToCSV(walker)
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

package cz.cvut.fit.prl.scalaimplicit.counter

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import io.circe.generic.auto._
object Main extends LazyLogging {

  def main(args: Array[String]): Unit = {
    val config = Cli(args)
    config match {
      case Some(conf) => {
        logger.debug(s"Reencoding ${conf.source}")
        val res = Json4sDecoder.loadJSON(conf.source)
        JSONSerializer.saveJSON(res, conf.outdir + "/results.json")
        JSONSerializer.saveJSON(res.callSites, conf.outdir + "/results-callsites.json")
        JSONSerializer.saveJSON(res.declarations, conf.outdir + "/results-declarations.json")
      }
      case None => {
        println("No arguments found. Closing")
      }
    }
  }
}

object Json4sDecoder {
  import Representation._
  import org.json4s._
  import org.json4s.native.Serialization
  import org.json4s.native.Serialization.read

  def loadJSON(file: String): ImplicitAnalysisResult = {
    implicit val formats = Serialization.formats(
      ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))
    val source = scala.io.Source.fromFile(file).mkString
    read[ImplicitAnalysisResult](source)
  }
}
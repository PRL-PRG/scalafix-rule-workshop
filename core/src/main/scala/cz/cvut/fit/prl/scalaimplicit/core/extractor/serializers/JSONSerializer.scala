package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation

object JSONSerializer {
  import Representation._
  import org.json4s._
  import org.json4s.native.JsonMethods._
  import org.json4s.native.Serialization
  import org.json4s.native.Serialization.{read, write}

  def saveJSON(res: ImplicitAnalysisResult, file: String) = {
    implicit val formats = Serialization.formats(
      ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))
    val ser = write(res)
    Files.write(Paths.get(file), ser.getBytes)
  }

  def loadJSON(file: String): ImplicitAnalysisResult = {
    implicit val formats = Serialization.formats(
      ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))
    val source = io.Source.fromFile(file).mkString
    read[ImplicitAnalysisResult](source)
  }

  def prettyJSON(res: ImplicitAnalysisResult): String = {
    implicit val formats = Serialization.formats(
      ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))
    pretty(render(parse(write(res))))
  }
}

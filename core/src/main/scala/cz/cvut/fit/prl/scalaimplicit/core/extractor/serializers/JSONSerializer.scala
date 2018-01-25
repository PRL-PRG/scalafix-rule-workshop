package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation

object JSONSerializer {
  import Representation._
  import org.json4s._
  import org.json4s.native.JsonMethods._
  import org.json4s.native.Serialization
  import org.json4s.native.Serialization.{read, write}

  def saveJSON(res: ExtractionResult, file: String) = {
    implicit val formats = Serialization.formats(
      ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))
    val ser = write(res)
    Files.write(Paths.get(file), ser.getBytes)
  }

  def loadJSON(file: String): ExtractionResult = {
    implicit val formats = Serialization.formats(
      ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))
    val source = io.Source.fromFile(file).mkString
    read[ExtractionResult](source)
  }

  def prettyJSON(res: ExtractionResult): String = {
    implicit val formats = Serialization.formats(
      ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))
    pretty(render(parse(write(res))))
  }
}

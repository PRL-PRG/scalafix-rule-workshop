package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import java.nio.file.{Files, Path, Paths}

import io.circe.parser._
import io.circe.syntax._
import cats.syntax.show._
import io.circe.{Decoder, Encoder}
import io.circe._
import cz.cvut.fit.prl.scalaimplicit.core.util.Scala212Backport.Either212

object JSONSerializer {
  def saveJSON[T: Encoder](res: T, file: String): Path = {
    val ser = res.asJson
    Files.write(Paths.get(file), ser.noSpaces.getBytes)
  }

  def loadJSON[T: Decoder](file: String): T = {
    val res = for {
      json <- parse(scala.io.Source.fromFile(file).mkString)
      obj <- json.as[T]
    } yield obj

    res match {
      case Left(err) =>
        sys.error(
          "Unable to parse file: " + file + ": Error(" + err.show + ")")
      case Right(r) => r
    }
  }

  def prettyJSON[T: Encoder](res: T): String = res.asJson.spaces2
}
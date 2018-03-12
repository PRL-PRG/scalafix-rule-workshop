package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import java.nio.file.Files.{size, write}

import java.nio.file.{Files, Path, Paths}

import java.util.stream.Collectors

import com.typesafe.scalalogging.LazyLogging

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  Argument,
  ImplicitArgument
}

import io.circe.generic.auto._

import io.circe.syntax._

import org.json4s.native.Serialization

import org.json4s.native.Serialization.read

import org.json4s.{FileInput, ShortTypeHints}

import scala.collection.JavaConverters._

import scala.util.{Failure, Try}

object SchemaConverter extends App with LazyLogging {

  val Root =
    "/var/lib/scala/projects/"

  val depth = 4
  val InputFile = "results.json"
  val OutputCallSitesFile = "results-callsites.json"
  val OutputDeclarationsFile = "results-declarations.json"


  implicit val formats = Serialization.formats(
    ShortTypeHints(List(classOf[Argument], classOf[ImplicitArgument])))

  val paths =
    Files
      .find(Paths.get(Root), depth, (path, _) => path.endsWith(InputFile))
      .collect(Collectors.toSet[Path])
      .asScala
      .toSeq

  val res = paths.par.map { path =>
    logger.info(s"Loading $path ${size(path)} bytes")

    val callSiteFile = path.getParent.resolve(OutputCallSitesFile)
    val declarationsFile = path.getParent.resolve(OutputDeclarationsFile)

    Try(read[ImplicitAnalysisResult](FileInput(path.toFile)))
      .flatMap(
        res => { 
          logger.info(s"Finished reading $path")
          Try(write(callSiteFile, res.callSites.asJson.noSpaces.getBytes))
            .flatMap(_ => {
              logger.info(s"Call Sites written for $path")
              Try(write(declarationsFile,
                        res.declarations.asJson.noSpaces.getBytes))
              })
          })
  }.seq

  val failures = res zip paths collect {
    case (Failure(e), path) => path -> e
  }

  failures.foreach {
    case (path, e) =>
      println("PATH: " + path)
      println(e)
      println()

  }

  println(s"** Finished - failed ${failures.size} / ${paths.size}\n")

}

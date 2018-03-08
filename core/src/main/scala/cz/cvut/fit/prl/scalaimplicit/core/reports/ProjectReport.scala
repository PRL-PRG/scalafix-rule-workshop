package cz.cvut.fit.prl.scalaimplicit.core.reports

import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.SlimRepresentation.SlimResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import org.json4s
import org.json4s.JsonAST.{JArray, JField, JObject, JString}
import org.json4s.native.JsonMethods.parse
import io.circe.generic.auto._

case class ProjectReport(
    metadata: ProjectMetadata,
    result: ExtractionResult,
    stats: Statistics = Statistics.Default
)

// TODO This could probably go into queries
case class Statistics(
    percentageCovered: Double,
    callSitesAfterFilter: Int,
    callSitesBeforeFilter: Int
)
object Statistics {
  val Default = Statistics(1.0, 0, 0)
}

object ProjectReport extends LazyLogging {
  def loadReportsFromManifest(path: String): Seq[ProjectReport] = {
    val manifest = parse(
      Files.readAllLines(Paths.get(path)).toArray.mkString(""))
    manifest.children match {
      case List(JArray(children)) =>
        children.toParArray.map {
          case JObject(
              List(JField("metadata", JString(metadataPath)),
                   JField("results", JString(resultsPath)),
                   JField("paths", JString(pathsPath)))) =>
            logger.debug(s"Loading ${resultsPath}")
            ProjectReport(ProjectMetadata.loadFromCSV(metadataPath, pathsPath),
              JSONSerializer.loadJSON[ExtractionResult](resultsPath))
        }.arrayseq
    }
  }
}

case class SlimReport(
    metadata: ProjectMetadata,
    result: SlimResult,
    stats: Statistics = Statistics.Default
)
object SlimReport extends LazyLogging {
  def apply(report: ProjectReport): SlimReport =
    new SlimReport(report.metadata, SlimResult(report.result), report.stats)

  def loadFromManifest(path: String): Seq[SlimReport] =
    ProjectReport.loadReportsFromManifest(path).map(x => SlimReport(x))
}

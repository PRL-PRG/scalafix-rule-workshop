package cz.cvut.fit.prl.scalaimplicit.core.reports

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.SlimRepresentation.SlimResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import org.json4s.FileInput
import org.json4s.JsonAST.{JArray, JField, JObject, JString}
import org.json4s.native.JsonMethods

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
    val manifest = JsonMethods.parse(FileInput(new File(path)))

    manifest.children match {
      case List(JArray(children)) =>
        children.par.map {
          case JObject(
              List(JField("metadata", JString(metadataPath)),
                   JField("results", JString(resultsPath)))) =>

            logger.debug(s"Loading $resultsPath")

            ProjectReport(ProjectMetadata.loadFromCSV(metadataPath),
                          JSONSerializer.loadJSON(resultsPath))
        }.seq
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

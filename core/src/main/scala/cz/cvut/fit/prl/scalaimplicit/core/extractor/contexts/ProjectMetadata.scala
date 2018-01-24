package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ProjectReport.logger
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SlimRepresentation.{
  SlimCallSite,
  SlimResult
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import org.json4s._
import org.json4s.native.JsonMethods._

case class ProjectReport(
    metadata: ProjectMetadata,
    result: ExtractionResult
)

object ProjectReport extends LazyLogging {
  def loadFromManifest(path: String): Seq[ProjectReport] = {
    val manifest = parse(
      Files.readAllLines(Paths.get(path)).toArray.mkString(""))
    manifest.children match {
      case List(JArray(children)) =>
        children.toParArray.map {
          case JObject(
              List(JField("metadata", JString(metadataPath)),
                   JField("results", JString(resultsPath)))) =>
            logger.debug(s"Loading ${resultsPath}")
            ProjectReport(ProjectMetadata.loadFromCSV(metadataPath),
                          JSONSerializer.loadJSON(resultsPath))
        }.arrayseq
    }
  }
}

case class ReportSummary(
    reponame: String,
    callSites: Seq[(String, Int)],
    totalCallSites: Int,
    definitions: Seq[(String, Int)],
    totalDefinitions: Int
) {
  def twoCols =
    callSites.map(x => Some(x)) zipAll (definitions.map(x => Some(x)), None, None)
}
object ReportSummary {
  def apply(parts: Seq[ReportSummary]) = {
    parts.tail.fold(parts.head)(
      (report, acc) =>
        acc.copy(
          callSites = acc.callSites ++ report.callSites,
          totalCallSites = acc.totalCallSites + report.totalCallSites,
          definitions = acc.definitions ++ report.definitions,
          totalDefinitions = acc.totalDefinitions + report.totalDefinitions,
          reponame = ""
      ))
  }
  def apply(report: SlimReport): ReportSummary = {
    def groupAndCount(
        what: Iterable[{ def name: String }]): (Seq[(String, Int)], Int) = {
      val groups = what
        .groupBy(_.name)
        .map(x => (x._1, x._2.size))
        .toSeq
        .sortBy(-_._2)
      (groups, groups.map(x => x._2).sum)
    }
    val css = groupAndCount(report.result.callSites)
    val decls = groupAndCount(report.result.definitions)

    ReportSummary(report.metadata.reponame, css._1, css._2, decls._1, decls._2)
  }
}

case class SlimReport(
    metadata: ProjectMetadata,
    result: SlimResult
)
object SlimReport extends LazyLogging {
  def apply(report: ProjectReport): SlimReport =
    new SlimReport(report.metadata, SlimResult(report.result))

  def loadFromManifest(path: String): Seq[SlimReport] = {
    val manifest = parse(
      Files.readAllLines(Paths.get(path)).toArray.mkString(""))
    manifest.children match {
      case List(JArray(children)) =>
        children.toParArray.map {
          case JObject(
              List(JField("metadata", JString(metadataPath)),
                   JField("results", JString(resultsPath)))) =>
            logger.debug(s"Loading ${resultsPath}")
            SlimReport(
              ProjectReport(ProjectMetadata.loadFromCSV(metadataPath),
                            JSONSerializer.loadJSON(resultsPath)))
        }.arrayseq
    }
  }
}

case class ProjectMetadata(
    reponame: String,
    name: String,
    url: String,
    lastCommit: String,
    buildSystem: String,
    version: String,
    ghStars: Int,
    totalLOC: Int,
    scalaLOC: Int
)

object ProjectMetadata {
  def loadFromCSV(path: String): ProjectMetadata = {
    val lines: Seq[String] = io.Source.fromFile(path).getLines().toSeq
    val info = (lines(0).split(",") zip lines(1).split(",")).groupBy(_._1)
    ProjectMetadata(
      reponame = info("reponame").head._2,
      name = info("name").head._2,
      url = info("url").head._2,
      lastCommit = info("last_commit").head._2,
      buildSystem = info("build_system").head._2,
      version = info("version").head._2,
      ghStars = info("gh_stars").head._2.toInt,
      totalLOC = info("total_loc").head._2.toInt,
      scalaLOC = info("scala_loc").head._2.toInt
    )
  }
}

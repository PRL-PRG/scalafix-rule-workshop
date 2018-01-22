package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import org.json4s._
import org.json4s.native.JsonMethods._

case class ProjectReport(
    metadata: ProjectMetadata,
    result: ExtractionResult
)

object ProjectReport {
  def loadFromManifest(path: String): Seq[ProjectReport] = {
    val manifest = parse(
      Files.readAllLines(Paths.get(path)).toArray.mkString(""))
    manifest.children match {
      case List(JArray(children)) =>
        children map {
          case JObject(
              List(JField("metadata", JString(metadataPath)),
                   JField("results", JString(resultsPath)))) =>
            ProjectReport(ProjectMetadata.loadFromCSV(metadataPath),
                          JSONSerializer.loadJSON(resultsPath))
        }
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

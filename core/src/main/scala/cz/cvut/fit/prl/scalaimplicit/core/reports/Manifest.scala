package cz.cvut.fit.prl.scalaimplicit.core.reports

import java.nio.file.{Files, Paths}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.JSONSerializer
import org.json4s.JsonAST.{JArray, JField, JObject, JString}
import org.json4s.native.JsonMethods.parse
import io.circe.generic.auto._

case class Manifests(projects: Seq[ProjectManifest])
case class ProjectManifest(
    metadata: String,
    results: String,
    paths: String
) {
  val resultsFile = s"$results/results.json"
  val callSitesFile = s"$results/results-callsites.json"
  val definitionsFile = s"$results/results-declarations.json"
}

object Manifests extends LazyLogging {
  def fromJSON(path: String): Manifests =
    JSONSerializer.loadJSON[Manifests](path)
}

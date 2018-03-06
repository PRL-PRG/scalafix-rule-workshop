package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  Argument,
  ImplicitArgument
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer.TCFamily
import cz.cvut.fit.prl.scalaimplicit.core.reports.{
  DefinitionSummary,
  ReportSummary,
  SlimReport
}
import org.json4s.{NoTypeHints, ShortTypeHints}
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

object OutputHelper {
  def printSlimCallSiteReports(folder: String,
                               data: (Seq[SlimReport], Seq[SlimReport])) = {
    if (!Files.exists(Paths.get(folder)))
      Files.createDirectory(Paths.get(folder))
    writeReport(folder, "results", data._1)
    writeReport(folder, "excluded", data._2)
  }

  def writeReport(folder: String, prefix: String, data: Seq[SlimReport]) = {
    Files.write(
      Paths.get(s"./${folder}/${prefix}.coderefs.html"),
      HTMLSerializer
        .createSlimDocument(data, HTMLSerializer.CoderefDocument$)
        .getBytes
    )
    Files.write(
      Paths.get(s"./${folder}/${prefix}.summary.html"),
      HTMLSerializer
        .createSlimDocument(data, HTMLSerializer.SummaryDocument$)
        .getBytes
    )
    Files.write(
      Paths.get(s"./${folder}/${prefix}.project.summary.csv"),
      projectCSVSummary(data).getBytes
    )
    Files.write(
      Paths.get(s"./${folder}/${prefix}.callsite.summary.csv"),
      callSiteCSVSummary(data.map(x => ReportSummary(x))).getBytes
    )
  }

  def projectCSVSummary(reports: Seq[SlimReport]): String = {
    def prepareValue(x: String) = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    val header = "project, call_sites_before_filter, call_sites_after_filter"
    val values = reports
      .map(report =>
        s"""${prepareValue(report.metadata.reponame)},${prepareValue(
             report.stats.callSitesBeforeFilter.toString)},${prepareValue(
             report.stats.callSitesAfterFilter.toString)}""".stripMargin)
      .mkString("\n")
    s"$header\n$values"
  }

  def callSiteCSVSummary(projectSummaries: Seq[ReportSummary]): String = {
    def pV(x: String): String = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    def bV(x: Boolean): String = if (x) "T" else "F"

    val header = "project, name, occurrences, transitive"
    val values = projectSummaries
      .flatMap(
        report =>
          report.sortedCallSites
            .map(row =>
              s"""${pV(report.reponame)}, ${row.name}, ${row.occurrences}, ${bV(
                row.isTransitive)}""")
      )
      .mkString("\n")
    s"$header\n$values"
  }

  def definitionCSVSummary(projectSummaries: Seq[DefinitionSummary]): String = {
    def pV(x: String): String = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    def bV(x: Boolean): String = if (x) "T" else "F"

    val header = "project, sloc, name, occurrences, call_density"
    val values = projectSummaries
      .flatMap(
        report =>
          report.definitions
            .map(row =>
              s"""${pV(report.metadata.reponame)}, ${report.metadata.scalaLOC}, ${row._1}, ${row._2}, ${row._2.toDouble / report.metadata.scalaLOC.toDouble}""")
      )
      .mkString("\n")
    s"$header\n$values"
  }

  def TCFamiliesJSONSummary(data: Seq[TCFamily]): String = {
    implicit val formats = Serialization.formats(NoTypeHints)
    write(data)
  }

}

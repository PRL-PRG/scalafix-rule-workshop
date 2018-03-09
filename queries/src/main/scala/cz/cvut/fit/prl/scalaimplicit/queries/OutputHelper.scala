package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer.TCFamily
import cz.cvut.fit.prl.scalaimplicit.core.reports.{
  DefinitionSummary,
  ProjectReport,
  ReportSummary
}
import org.json4s.NoTypeHints
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

object OutputHelper {
  def printCallSiteReports(folder: String, data: Seq[ProjectReport]) = {
    if (!Files.exists(Paths.get(folder)))
      Files.createDirectory(Paths.get(folder))
    writeReport(folder, "results", data)
  }

  def writeReport(folder: String, prefix: String, data: Seq[ProjectReport]) = {
    Files.write(
      Paths.get(s"./${folder}/${prefix}.coderefs.html"),
      HTMLSerializer
        .createDocument(data, HTMLSerializer.CoderefDocument$)
        .getBytes
    )
    Files.write(
      Paths.get(s"./${folder}/${prefix}.summary.html"),
      HTMLSerializer
        .createDocument(data, HTMLSerializer.SummaryDocument$)
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
    Files.write(
      Paths.get(s"./${folder}/${prefix}.callsite.report.csv"),
      callSiteCSVReport(data).getBytes
    )
  }

  def projectCSVSummary(reports: Seq[ProjectReport]): String = {
    def prepareValue(x: String) = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    val header = "project, call_sites_after_filter"
    val values = reports
      .map(report =>
        s"""${prepareValue(report.metadata.reponame)},${prepareValue(
             report.result.callSites.size.toString)}""".stripMargin)
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

  def callSiteCSVReport(reports: Seq[ProjectReport]): String = {
    def pV(x: String): String = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    def bV(x: Boolean): String = if (x) "T" else "F"

    val header = "project, name, transitive, param_type, return_type"
    val values = reports
      .flatMap(
        report =>
          report.result.callSites
            .map(row =>
              Seq[String](
                pV(report.metadata.reponame),
                '"' + row.name + '"',
                '"' + bV(row.declaration.location.isEmpty) + '"',
                '"' + row.declaration.signature.get.parameterLists
                  .map(list => s"(${list.params.map(_.tipe).mkString(",")})")
                  .mkString(",") + '"',
                '"' + row.declaration.signature.get.returnType.get.toString + '"'
              ).mkString(","))
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

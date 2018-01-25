package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer
import cz.cvut.fit.prl.scalaimplicit.core.reports.SlimReport

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
      Paths.get(s"./${folder}/${prefix}.summary.csv"),
      csvSummary(data).getBytes
    )
  }

  def csvSummary(reports: Seq[SlimReport]): String = {
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

}

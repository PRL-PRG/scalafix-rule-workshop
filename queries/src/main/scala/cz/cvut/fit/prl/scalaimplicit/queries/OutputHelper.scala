package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer
import cz.cvut.fit.prl.scalaimplicit.core.reports.{ProjectReport, ReportSummary}
import cz.cvut.fit.prl.scalaimplicit.schema.Type

trait Reporter {
  def writeReports(folder: String, prefix: String, data: Seq[ProjectReport])
}

object OutputHelper {
  def printReports(folder: String,
                   data: Seq[ProjectReport],
                   reporter: Reporter,
                   reporters: Reporter*) = {
    if (!Files.exists(Paths.get(folder)))
      Files.createDirectory(Paths.get(folder))

    (reporter +: reporters).foreach(_.writeReports(folder, "results", data))
  }

  object ValuePreppers {
    def sV(x: String) = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    def bV(x: Boolean): String = if (x) "T" else "F"

    def printType(t: Type): String =
      s"${t.name}[${t.parameters.map(x => "_").mkString(",")}]"

  }

  object ProjectReporter extends Reporter {

    def writeReports(folder: String,
                     prefix: String,
                     data: Seq[ProjectReport]) = {
      Files.write(
        Paths.get(s"./${folder}/${prefix}.project.summary.csv"),
        projectCSVSummary(data).getBytes
      )
    }

    def projectCSVSummary(reports: Seq[ProjectReport]): String = {

      val header = "project, call_sites_after_filter"
      val values = reports
        .map(report =>
          s"""${ValuePreppers.sV(report.metadata.reponame)},${ValuePreppers.sV(
               report.result.callSites.size.toString)}""".stripMargin)
        .mkString("\n")
      s"$header\n$values"
    }
  }

  object CallSiteReporter extends Reporter {

    def writeReports(folder: String,
                     prefix: String,
                     data: Seq[ProjectReport]) = {
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
        Paths.get(s"./${folder}/${prefix}.callsite.summary.csv"),
        callSiteCSVSummary(data.map(x => ReportSummary(x))).getBytes
      )
      Files.write(
        Paths.get(s"./${folder}/${prefix}.callsite.report.csv"),
        callSiteCSVReport(data).getBytes
      )
    }

    def callSiteCSVSummary(projectSummaries: Seq[ReportSummary]): String = {

      val header = "project, name, occurrences, transitive"
      val values = projectSummaries
        .flatMap(
          report =>
            report.sortedCallSites
              .map(row =>
                s"""${ValuePreppers
                  .sV(report.reponame)}, ${row.name}, ${row.occurrences}, ${ValuePreppers
                  .bV(row.isTransitive)}""")
        )
        .mkString("\n")
      s"$header\n$values"
    }

    def callSiteCSVReport(reports: Seq[ProjectReport]): String = {

      val header = "project, name, transitive, param_type, return_type"
      val values = reports
        .flatMap(
          report =>
            report.result.callSites
              .map(row =>
                Seq[String](
                  ValuePreppers.sV(report.metadata.reponame),
                  '"' + row.name + '"',
                  '"' + ValuePreppers
                    .bV(row.declaration.location.isEmpty) + '"',
                  '"' + row.declaration.signature.get.parameterLists
                    .map(list =>
                      s"(${list.parameters.map(p => ValuePreppers.printType(p.parameterType)).mkString(",")})")
                    .mkString(",") + '"',
                  '"' + ValuePreppers.printType(row.declaration.signature.get.returnType) + '"'
                ).mkString(","))
        )
        .mkString("\n")
      s"$header\n$values"
    }
  }

  object DeclarationReporter extends Reporter {

    def writeReports(folder: String,
                     prefix: String,
                     data: Seq[ProjectReport]) = {
      Files.write(
        Paths.get(s"./${folder}/${prefix}.definition.summary.csv"),
        definitionCSVSummary(data.map(x => ReportSummary(x))).getBytes
      )
      Files.write(
        Paths.get(s"./${folder}/${prefix}.definition.report.csv"),
        definitionCSVReport(data).getBytes
      )
    }

    def definitionCSVSummary(projectSummaries: Seq[ReportSummary]): String = {

      val header = "project, name, occurrences"
      val values = projectSummaries
        .flatMap(
          report =>
            report.definitions
              .map(row =>
                s"""${ValuePreppers
                  .sV(report.reponame)}, ${row.name}, ${row.occurrences}""")
        )
        .mkString("\n")
      s"$header\n$values"
    }

    def definitionCSVReport(reports: Seq[ProjectReport]): String = {
      val header = "project, name, param_type, return_type"
      val values = reports
        .flatMap(
          report =>
            report.result.declarations
              .map(row =>
                Seq[String](
                  ValuePreppers.sV(report.metadata.reponame),
                  '"' + row.name + '"',
                  '"' + row.signature.get.parameterLists
                    .map(list =>
                      s"(${list.parameters.map(p => ValuePreppers.printType(p.parameterType)).mkString(",")})")
                    .mkString(",") + '"',
                  '"' + ValuePreppers.printType(row.signature.get.returnType) + '"'
                ).mkString(","))
        )
        .mkString("\n")
      s"$header\n$values"
    }
  }
}

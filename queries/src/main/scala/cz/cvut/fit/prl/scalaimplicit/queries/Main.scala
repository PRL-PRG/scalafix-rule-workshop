package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SlimRepresentation.SlimDefinition
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  DefinitionCount,
  ProjectReport,
  SlimReport
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer

object Main {

  def dumpAll() = {
    val res =
      SlimReport.loadFromManifest("../top-120-results/results/manifest.json")

    printSlimCallSiteReports("tmp/all", res)
  }

  def conversion(): Unit = {
    val res =
      ProjectReport.loadFromManifest(
        "../top-120-results/results/manifest.json")
    // Fiter queries
    val qres = QueryEngine(
      {
        case CallSite(
            _,
            _,
            _,
            true,
            Declaration(
              _,
              kind,
              _,
              true,
              Some(
                Signature(_,
                          Seq(DeclaredParameterList(Seq(parameter), false)),
                          _)),
              _),
            _,
            _) if (kind.contains("def") || kind.contains("class")) =>
          true
        case _ => false
      },
      res
    )
    printSlimCallSiteReports("tmp/conversion", qres.map(x => SlimReport(x)))
  }

  def typeClass() = {
    val res =
      ProjectReport.loadFromManifest(
        "../top-120-results/results/manifest.json")

    val qres = QueryEngine(
      {
        case CallSite(_, _, _, _, _, _, iargs)
            if QueryEngine.contains[ArgumentLike](
              iargs, {
                case arg: ImplicitArgument =>
                  (arg.declaration.isImplicit
                    && QueryEngine.matches[String](
                      arg.declaration.kind,
                      k => k.contains("def") || k.contains("object"))
                    && QueryEngine.matches[Option[Type]](
                      arg.declaration.signature.get.returnType,
                      rt => rt.isDefined && rt.get.parameters.isEmpty)
                    && QueryEngine.contains[Parent](
                      arg.declaration.parents,
                      parent =>
                        QueryEngine.matches[Declaration](
                          parent.declaration,
                          d =>
                            d.kind
                              .contains("trait") && d.signature.get.typeParams.size == 1)
                          && parent.typeArguments.size == 1
                    ))
                case _ => false
              }
            ) =>
          true
        case _ => false
      },
      res
    )
    printSlimCallSiteReports("tmp/typeclass", qres.map(x => SlimReport(x)))
  }

  def declarationsByCallSite() = {
    val res =
      ProjectReport.loadFromManifest(
        "../top-120-results/results/manifest.json")

    val decls: Seq[DefinitionCount] = res.map(proj => {
      DefinitionCount(
        proj.metadata,
        proj.result.callSites
          .flatMap(cs =>
            cs.implicitArguments.collect {
              case arg: ImplicitArgument if arg.declaration.isImplicit =>
                (SlimDefinition(arg.declaration), 1)
          })
          .groupBy(_._1.kindedName)
          .map(d => d._1 -> d._2.size)
      )
    })

    Files.write(
      Paths.get("./tmp/contextcandidates/definitions.html"),
      HTMLSerializer
        .createSlimDocument[DefinitionCount](decls,
                                             HTMLSerializer.DefinitionReport)
        .getBytes
    )
  }

  def moreThanOneParam(): Unit = {
    val res =
      ProjectReport.loadFromManifest(
        "../top-120-results/results/manifest.json")

    val qres = QueryEngine((cs) => cs.implicitArguments.size > 1, res)
    printSlimCallSiteReports("tmp/morethanone", qres.map(x => SlimReport(x)))
  }

  def main(args: Array[String]): Unit = {
    //doSpark()

    //dumpAll()
    //conversion()
    //typeClass()
    //declarationsByCallSite()
    moreThanOneParam()
  }

  def printSlimCallSiteReports(folder: String, data: Seq[SlimReport]) = {
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

    Files.write(
      Paths.get(s"./${folder}/coderefs.html"),
      HTMLSerializer
        .createSlimDocument(data, HTMLSerializer.CoderefReport)
        .getBytes
    )
    Files.write(
      Paths.get(s"./${folder}/summary.html"),
      HTMLSerializer
        .createSlimDocument(data, HTMLSerializer.SummaryReport)
        .getBytes
    )
    Files.write(
      Paths.get(s"./${folder}/summary.csv"),
      csvSummary(data).getBytes
    )
  }
}

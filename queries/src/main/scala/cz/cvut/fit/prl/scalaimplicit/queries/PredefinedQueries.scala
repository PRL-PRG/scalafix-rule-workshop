package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.SlimRepresentation.SlimDefinition
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer
import cz.cvut.fit.prl.scalaimplicit.core.reports.{
  DefinitionSummary,
  ProjectReport,
  SlimReport
}
import cz.cvut.fit.prl.scalaimplicit.queries.QueryEngine.CSFilterQuery

object PredefinedQueries {

  val DATASET =
    ProjectReport.loadReportsFromManifest("../test_repos/manifest.json")

  import OutputHelper._

  def dumpAll() = {
    query("tmp", Seq(CSFilterQuery("all", x => true)))
  }

  val conversionFunction: CallSite => Boolean = {
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
  }

  def conversion(): Unit = {
    query(
      "tmp",
      Seq(CSFilterQuery("conversion", conversionFunction))
    )
  }

  val nonTransitiveFunction: CallSite => Boolean = {
    case CallSite(_, _, _, _, Declaration(_, _, Some(_), _, _, _), _, _) =>
      true
    case _ => false
  }

  def nonTransitiveConversion() = {
    query(
      "tmp",
      Seq(CSFilterQuery("conversion", conversionFunction),
          CSFilterQuery("nontransitive", nonTransitiveFunction))
    )
  }

  def typeClass() = {
    query(
      "tmp",
      Seq(
        CSFilterQuery(
          "typeclass", {
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
          }
        ))
    )
  }

  def declarationsByCallSite() = {
    val res = DATASET
    val decls: Seq[DefinitionSummary] = res.map(proj => {
      DefinitionSummary(
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
        .createSlimDocument[DefinitionSummary](
          decls,
          HTMLSerializer.DefinitionDocument$)
        .getBytes
    )
  }

  def moreThanOneParam(): Unit = {
    query("tmp",
          Seq(CSFilterQuery("morethanone", _.implicitArguments.size > 1)))
  }

  def query(outfolder: String,
            queries: Seq[QueryEngine.FilterQuery[CallSite]]): Unit = {
    def makeQuery(reports: Seq[ProjectReport],
                  path: String,
                  queries: Seq[QueryEngine.FilterQuery[CallSite]]): Unit = {
      if (queries.nonEmpty) {
        val query = queries.head
        val newPath = s"$path/${query.name}"
        val qres = QueryEngine(query, reports)
        printSlimCallSiteReports(
          newPath,
          (qres._1.map(SlimReport(_)), qres._2.map(SlimReport(_))))
        makeQuery(qres._1, newPath, queries.tail)
      }
    }

    makeQuery(DATASET, outfolder, queries)
  }
}

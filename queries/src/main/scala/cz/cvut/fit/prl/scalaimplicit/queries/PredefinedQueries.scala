package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  DefinitionCount,
  ProjectReport,
  SlimReport
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SlimRepresentation.SlimDefinition
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.HTMLSerializer
import cz.cvut.fit.prl.scalaimplicit.queries.QueryEngine.CSFilterQuery

object PredefinedQueries {

  import OutputHelper._

  def dumpAll() = {
    loadQueryPrint("../top-120-results/results/manifest.json",
                   "tmp/all",
                   CSFilterQuery(x => true))
  }

  def conversion(): Unit = {
    loadQueryPrint(
      "../top-120-results/results/manifest.json",
      "tmp/conversion",
      CSFilterQuery(
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
        }
      )
    )
  }

  def typeClass() = {
    loadQueryPrint(
      "../top-120-results/results/manifest.json",
      "tmp/typeclass",
      CSFilterQuery({
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
      })
    )
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
    loadQueryPrint("../top-120-results/results/manifest.json",
                   "tmp/morethanone",
                   CSFilterQuery(_.implicitArguments.size > 1))
  }

  def loadQueryPrint(manifestPath: String,
                     outfolder: String,
                     query: QueryEngine.FilterQuery[CallSite]): Unit = {
    val res =
      ProjectReport.loadFromManifest(manifestPath)

    val qres = QueryEngine(query, res)
    printSlimCallSiteReports(
      outfolder,
      (qres._1.map(SlimReport(_)), qres._2.map(SlimReport(_))))
  }
}

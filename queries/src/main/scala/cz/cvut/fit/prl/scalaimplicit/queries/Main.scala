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

    printSlimHTML("tmp/all", res)
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
              "def",
              _,
              true,
              Some(
                Signature(_,
                          Seq(DeclaredParameterList(Seq(parameter), false)),
                          _)),
              _),
            _,
            _) =>
          true
        case _ => false
      },
      res
    )
    printSlimHTML("tmp/conversion", qres.map(x => SlimReport(x)))
  }

  def typeClass() = {
    val res =
      ProjectReport.loadFromManifest(
        "../top-120-results/results/manifest.json")
    // Fiter queries
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
    printSlimHTML("tmp/typeclass", qres.map(x => SlimReport(x)))
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

  def main(args: Array[String]): Unit = {
    //doSpark()

    dumpAll()
    conversion()
    typeClass()
    declarationsByCallSite()
  }

  def printSlimHTML(folder: String, data: Seq[SlimReport]) = {
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
  }
}

package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult
import cz.cvut.fit.prl.scalaimplicit.core.reports._
import cz.cvut.fit.prl.scalaimplicit.matcher._
import cz.cvut.fit.prl.scalaimplicit.queries.OutputHelper.{CallSiteReporter, DeclarationReporter, ProjectReporter}
import cz.cvut.fit.prl.scalaimplicit.query.JsonQuery
import cz.cvut.fit.prl.scalaimplicit.schema._

object PredefinedQueries extends Matchers with SchemaMatchers {

  val DATASET =
    Manifests.fromJSON("../test_repos/manifest.json")

  val OUTFOLDER = "../tmp/results/rawdata/small"

  private case class QueryResult[A](result: Seq[A], metadata: ProjectMetadata)
  private def queryDeclarations(
      matcher: Matcher[Declaration]): Seq[QueryResult[Declaration]] = {
    DATASET.projects.par
      .map(
        project =>
          QueryResult(
            JsonQuery.query(project.definitionsFile, matcher),
            ProjectMetadata.loadFromCSV(project.metadata, project.paths)))
      .seq
  }

  private def printDeclarations(results: Seq[QueryResult[Declaration]],
                                destination: String): Unit = {
    OutputHelper.printReports(
      s"$OUTFOLDER/$destination",
      results.map(
        result =>
          ProjectReport(result.metadata,
                        ImplicitAnalysisResult(Seq(), result.result.toSet))),
      ProjectReporter,
      DeclarationReporter
    )
  }

  private def queryCallSites(
      matcher: Matcher[CallSite]): Seq[QueryResult[CallSite]] = {
    DATASET.projects.par
      .map(
        project => {
          println(project)
          QueryResult(
            JsonQuery.query(project.callSitesFile, matcher),
            ProjectMetadata.loadFromCSV(project.metadata, project.paths))
        }
      )
      .seq
  }

  private def queryCallSitesWithMetadata(
      matcher: (ProjectMetadata) => Matcher[CallSite])
    : Seq[QueryResult[CallSite]] = {
    DATASET.projects.par
      .map(project => {
        val metadata =
          ProjectMetadata.loadFromCSV(project.metadata, project.paths)
        QueryResult(JsonQuery.query(project.callSitesFile, matcher(metadata)),
                    metadata)
      })
      .seq
  }

  private def printCallSites(results: Seq[QueryResult[CallSite]],
                             destination: String): Unit = {
    OutputHelper.printReports(
      s"$OUTFOLDER/$destination",
      results.map(
        result =>
          ProjectReport(result.metadata,
                        ImplicitAnalysisResult(result.result, Set()))),
      ProjectReporter,
      CallSiteReporter
    )
  }

  object PredefinedMatchers {

    val all: Matcher[CallSite] =
      BooleanPropertyMatcher[CallSite]("all", x => true)

    val conversionDecl: Matcher[Declaration] =
      and(and(isImplicit, kind(in("def", "class"))),
          signature(
            parameterLists(
              size(1),
              contains(
                !isImplicit,
                parameters(size(1))
              )
            )
          ))

    val conversion: Matcher[CallSite] =
      and(
        isSynthetic,
        declaration(
          conversionDecl
        )
      )

    val transitive: Matcher[CallSite] = declaration(location(isEmpty))

    // TODO Probably this can be done with the contains() matcher
    def isPrefixIn(strings: Seq[String]): Matcher[String] =
      BooleanPropertyMatcher[String]("isPrefixIn",
                                     str => strings.exists(str.startsWith))
    def inMain(metadata: ProjectMetadata): Matcher[CallSite] =
      location(file(isPrefixIn(metadata.mainPaths)))
    def inTest(metadata: ProjectMetadata): Matcher[CallSite] =
      location(file(isPrefixIn(metadata.testPaths)))
  }

  def conversion(): Unit = {
    printCallSites(queryCallSites(PredefinedMatchers.conversion), "conversion")
  }

  def dumpAll() = {
    printCallSites(queryCallSites(PredefinedMatchers.all), "all")
  }

  def conversionTransitivity() = {
    printCallSites(
      queryCallSites(
        and(PredefinedMatchers.conversion, PredefinedMatchers.transitive)),
      "conversion/transitive")

    printCallSites(
      queryCallSites(
        and(PredefinedMatchers.conversion, !PredefinedMatchers.transitive)),
      "conversion/non-transitive")
  }

  def mainTest() = {
    printCallSites(queryCallSitesWithMetadata(PredefinedMatchers.inMain),
                   "in-main")

    printCallSites(queryCallSitesWithMetadata(PredefinedMatchers.inTest),
                   "in-test")
  }

  def conversionInMain(): Unit = {
    printCallSites(
      queryCallSitesWithMetadata(data =>
        and(PredefinedMatchers.inMain(data), PredefinedMatchers.conversion)),
      "conversion/in-main")
  }

  def conversionInTest(): Unit = {
    printCallSites(
      queryCallSitesWithMetadata(data =>
        and(PredefinedMatchers.inTest(data), PredefinedMatchers.conversion)),
      "conversion/in-test")
  }

  def conversionDefinitions(): Unit = {
    printDeclarations(queryDeclarations(PredefinedMatchers.conversionDecl),
                      "conversion")
  }

  /*
  def typeClassClassification() = {
    def parentCandidate(s: Declaration): Boolean = {
      s.signature.get.typeParams.nonEmpty &&
      (s.kind.endsWith("trait") || s.kind.endsWith("abstract class"))
    }
    def instanceCandidate(s: Declaration): Boolean = {
      s.isImplicit &&
      (s.kind.contains("object") || s.kind.contains("def") || s.kind.contains(
        "class")) &&
        s.signature.get.returnType.isDefined && s.signature.get.returnType.get.parameters.isEmpty
    }
    def isTCInstanceOf(parent: Declaration, inst: Declaration): Boolean = {
      inst.parents.exists(_.name == parent.name)
    }

    val data = DATASET
    val defns: Seq[(Declaration, ProjectMetadata)] =
      for {
        proj <- data
        defn <- proj.result.declarations
          .filter(d => parentCandidate(d) || instanceCandidate(d))
      } yield {
        defn -> proj.metadata
      }

    val families = (for {
      parent <- defns.filter(d => parentCandidate(d._1))
      children = defns.filter(x =>
        instanceCandidate(x._1) && isTCInstanceOf(parent._1, x._1))
    } yield {
      TCFamily(
        TCItem(SlimDefinition(parent._1), parent._2),
        children.map(
          child =>
            TCItem(
              SlimDefinition(child._1),
              child._2
          ))
      )
    }).filter(_.instances.nonEmpty)

    val outfile = OUTFOLDER + "/specialqueries/tcclassification"

    Files.write(
      Paths.get(outfile + ".html"),
      HTMLSerializer
        .createSlimDocument(families, HTMLSerializer.TCListDocument)
        .getBytes
    )

    Files.write(
      Paths.get(outfile + ".json"),
      OutputHelper.TCFamiliesJSONSummary(families).getBytes
    )
  }

  import OutputHelper._


  val nonTransitiveFunction: CallSite => Boolean = {
    case CallSite(_, _, _, _, Declaration(_, _, Some(_), _, _, _), _, _) =>
      true
    case _ => false
  }

  def typeClass() = {
    query(
      OUTFOLDER,
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
              case arg: ImplicitArgument
                  if arg.declaration.isImplicit && arg.typeArguments.isEmpty =>
                (SlimDefinition(arg.declaration), 1)
          })
          .groupBy(_._1.kindedName)
          .map(d => d._1 -> d._2.size)
      )
    })

    Files.write(
      Paths.get(OUTFOLDER + "/contextcandidates/definitions.html"),
      HTMLSerializer
        .createSlimDocument[DefinitionSummary](
          decls,
          HTMLSerializer.DefinitionDocument$)
        .getBytes
    )

    Files.write(
      Paths.get(OUTFOLDER + "/contextcandidates/definitions.csv"),
      OutputHelper.definitionCSVSummary(decls).getBytes()
    )
  }

  def moreThanOneParam(): Unit = {
    query(OUTFOLDER,
          Seq(CSFilterQuery("morethanone", _.implicitArguments.size > 1)))
  }

  def badConversions(): Unit = {
    def isPrimitive(name: String): Boolean = {
      name.isOneOF(
        "scala.Predef.String",
        "java.lang.String",
        "scala.Int",
        "scala.Float",
        "scala.Double",
        "scala.Short",
        "scala.Byte",
        "scala.Any",
        "scala.AnyVal"
      )
    }

    def hasPrimitiveParam(cs: CallSite): Boolean = {
      val sign = cs.declaration.signature.get

      sign.parameterLists.exists(list =>
        !list.isImplicit && list.params.exists(p => isPrimitive(p.tipe.name)))
    }

    def hasPrimitiveRetType(cs: CallSite): Boolean = {
      val sign = cs.declaration.signature.get

      isPrimitive(sign.returnType.get.name)
    }

    query(
      OUTFOLDER,
      Seq(CSFilterQuery("conversion", conversionFunction),
          CSFilterQuery("primitive-param", hasPrimitiveParam))
    )

    query(
      OUTFOLDER,
      Seq(CSFilterQuery("conversion", conversionFunction),
          CSFilterQuery("primitive-return", hasPrimitiveRetType))
    )

    query(
      OUTFOLDER,
      Seq(CSFilterQuery("conversion", conversionFunction),
          CSFilterQuery("primitive-both",
                        x => hasPrimitiveParam(x) && hasPrimitiveRetType(x)))
    )
  }*/
}

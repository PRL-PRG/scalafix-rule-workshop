package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.SlimRepresentation.SlimResult
import cz.cvut.fit.prl.scalaimplicit.core.reports._
import cz.cvut.fit.prl.scalaimplicit.matcher._
import cz.cvut.fit.prl.scalaimplicit.queries.OutputHelper.printSlimCallSiteReports
import cz.cvut.fit.prl.scalaimplicit.query.JsonQuery

object PredefinedQueries extends Matchers with SchemaMatchers {

  val DATASET =
    Manifests.fromJSON("../test_repos/manifest.json")

  val OUTFOLDER = "./results/rawdata/small"

  implicit class oneOf(what: String) {
    def isOneOF(args: String*): Boolean = args.contains(what)
    def isPrefixOfOneOf(args: String*): Boolean = args.exists(what.startsWith)
  }

  def conversion(): Unit = {
    import io.circe.generic.auto._
    val m: Matcher[CallSite] =
      and(
        isSynthetic,
        declaration(
          isImplicit,
          kind(in("def", "class")),
          signature(
            parameterLists(
              size(1),
              contains(
                !isImplicit,
                parameters(size(1))
              )
            )
          )
        )
      )
    val reports: Seq[(List[CallSite], ProjectMetadata)] =
      DATASET.projects.par
        .map(
          project =>
            JsonQuery.query(project.callSitesFile, m) ->
              ProjectMetadata.loadFromCSV(project.metadata, project.paths))
        .seq

    printSlimCallSiteReports(
      s"$OUTFOLDER/conversion",
      reports.map(
        report =>
          SlimReport.apply(report._2,
                           SlimResult(ImplicitAnalysisResult(report._1, Set())),
                           Statistics.Default))
    )
  }
  /*
  def inMain(param: (CallSite, ProjectMetadata)): Boolean = param match {
    case (cs: CallSite, metadata: ProjectMetadata) => {
      cs.location.get.file.isPrefixOfOneOf(metadata.mainPaths: _*)
    }
  }

  def inTest(param: (CallSite, ProjectMetadata)): Boolean = param match {
    case (cs: CallSite, metadata: ProjectMetadata) => {
      cs.location.get.file.isPrefixOfOneOf(metadata.testPaths: _*)
    }
  }

  def mainTest() = {
    query(OUTFOLDER, Seq(PathFilterQuery("in-main", inMain)))
    query(OUTFOLDER, Seq(PathFilterQuery("in-test", inMain)))
  }

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

  def dumpAll() = {
    query(OUTFOLDER, Seq(CSFilterQuery("all", x => true)))
  }

  def conversionInMain(): Unit = {
    query(
      OUTFOLDER,
      Seq(
        CSFilterQuery("conversion", conversionFunction),
        PathFilterQuery("in-main", inMain)
      )
    )
  }

  def conversionInTest(): Unit = {
    query(
      OUTFOLDER,
      Seq(
        CSFilterQuery("conversion", conversionFunction),
        PathFilterQuery("in-test", inTest)
      )
    )
  }

  val nonTransitiveFunction: CallSite => Boolean = {
    case CallSite(_, _, _, _, Declaration(_, _, Some(_), _, _, _), _, _) =>
      true
    case _ => false
  }

  def nonTransitiveConversion() = {
    query(
      OUTFOLDER,
      Seq(CSFilterQuery("conversion", conversionFunction),
          CSFilterQuery("nontransitive", nonTransitiveFunction))
    )
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
  }

  def query(outfolder: String, queries: Seq[FilterQuery[_]]): Unit = {
    def makeQuery[A](reports: Seq[ProjectReport],
                     path: String,
                     queries: Seq[FilterQuery[_]]): Unit = {
      if (queries.nonEmpty) {
        val query: FilterQuery[_] = queries.head
        val newPath = s"$path/${query.name}"
        // TODO Use proper polymorphism instead
        val qres = query match {
          case q: FilterQuery.CSFilterQuery => QueryEngine(q, reports)
          case q: FilterQuery.PathFilterQuery => QueryEngine(q, reports)
        }
        printSlimCallSiteReports(
          newPath,
          (qres._1.map(SlimReport(_)), qres._2.map(SlimReport(_))))
        makeQuery(qres._1, newPath, queries.tail)
      }
    }

    makeQuery(DATASET, outfolder, queries)
  }*/
}

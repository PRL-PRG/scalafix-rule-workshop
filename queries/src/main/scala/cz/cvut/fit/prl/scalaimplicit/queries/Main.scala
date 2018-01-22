package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ExtractionResult,
  contexts
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ProjectMetadata,
  ProjectReport
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters.PrettyInstances._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.{
  HTMLSerializer,
  JSONSerializer
}

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val res = ProjectReport.loadFromManifest("./tmp/manifest.json")
    //println(s"Found ${res.callSites.size} call sites")

    // Corpus-wide statistic queries
    val corpusqres =
      res
        .flatMap(proj => {
          proj.result.callSites
        })
        .groupBy(_.name)
        .map(x => (x._1, x._2.size))
        .toSeq
        .sortBy(_._2)
        .reverse

    println(s"Most frequent implicit: ${corpusqres.head.toString()}")

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
    printResHTML(qres)
  }

  def printCallSites(css: Seq[CallSite]) = {
    css.map(prettyPrint(_)(PrettyCallSite)).map(println)
  }

  def printResHTML(data: Seq[ProjectReport]) =
    Files.write(
      Paths.get("./tmp/res.html"),
      HTMLSerializer
        .createDocument(data)
        .getBytes
    )
}

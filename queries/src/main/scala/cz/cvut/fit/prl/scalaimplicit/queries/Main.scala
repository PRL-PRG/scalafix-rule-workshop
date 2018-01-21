package cz.cvut.fit.prl.scalaimplicit.queries

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ProjectMetadata
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters.PrettyInstances._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.{
  HTMLSerializer,
  JSONSerializer
}

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val res = JSONSerializer.loadJSON("./tmp/res.json")
    println(s"Found ${res.callSites.size} call sites")
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
    //printCallSites(qres)
    printResHTML(res)
  }

  def printCallSites(css: Seq[CallSite]) = {
    css.map(prettyPrint(_)(PrettyCallSite)).map(println)
  }

  def printResHTML(res: ExtractionResult) =
    Files.write(
      Paths.get("./tmp/res.html"), {
        val mockData = ProjectMetadata.loadFromCSV("project.csv")
        HTMLSerializer.createDocument(res, mockData)
      }.getBytes
    )
}

package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.JSONSerializer
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.PrettyPrinters._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.PrettyPrinters.PrettyInstances._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation._

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
    printCallSites(qres)
  }

  def printCallSites(css: Seq[CallSite]) = {
    css.map(prettyPrint(_)(PrettyCallSite)).map(println)
  }
}

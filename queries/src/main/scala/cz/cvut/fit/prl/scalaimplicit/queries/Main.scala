package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Serializer
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.PrettyPrinters._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.PrettyPrinters.PrettyInstances._

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val res = Serializer.load("./res.dat")
    println(s"Found ${res.callSites.size} call sites")
    res.callSites.map(prettyPrint(_)(PrettyCallSite))
  }
}

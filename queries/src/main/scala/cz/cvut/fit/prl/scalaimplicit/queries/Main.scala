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

  def main(args: Array[String]): Unit = {
    PredefinedQueries.dumpAll()
    PredefinedQueries.conversion()
    PredefinedQueries.typeClass()
    PredefinedQueries.declarationsByCallSite()
    PredefinedQueries.moreThanOneParam()
  }
}

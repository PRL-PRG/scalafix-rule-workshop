package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.matcher.PredefinedQueries

object Main {

  def main(args: Array[String]): Unit = {
    PredefinedQueries.dumpAll()
    PredefinedQueries.conversion()
    PredefinedQueries.nonTransitiveConversion()
    PredefinedQueries.typeClass()
    PredefinedQueries.declarationsByCallSite()
    PredefinedQueries.moreThanOneParam()
    PredefinedQueries.typeClassClassification()
  }
}

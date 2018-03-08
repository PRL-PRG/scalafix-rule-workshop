package cz.cvut.fit.prl.scalaimplicit.queries

object Main {

  def main(args: Array[String]): Unit = {
    PredefinedQueries.conversion()
    PredefinedQueries.dumpAll()
    PredefinedQueries.conversionTransitivity()
    PredefinedQueries.mainTest()
    PredefinedQueries.conversionInMain()
    PredefinedQueries.conversionInTest()
    //PredefinedQueries.declarationsByCallSite()
    //PredefinedQueries.typeClass()
    //PredefinedQueries.moreThanOneParam()
    //PredefinedQueries.typeClassClassification()
  }
}

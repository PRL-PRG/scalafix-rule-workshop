package cz.cvut.fit.prl.scalaimplicit.queries

object Main {

  def main(args: Array[String]): Unit = {
    PredefinedQueries.dumpAll()
    PredefinedQueries.conversion()
    PredefinedQueries.typeClass()
    PredefinedQueries.declarationsByCallSite()
    PredefinedQueries.moreThanOneParam()
  }
}

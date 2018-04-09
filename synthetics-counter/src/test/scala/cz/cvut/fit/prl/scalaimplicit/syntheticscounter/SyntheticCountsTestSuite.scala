package cz.cvut.fit.prl.scalaimplicit.syntheticscounter

import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import org.langmeta.semanticdb.Database

class SyntheticCountsTestSuite extends SemanticdbTest {

  def checkDB(name: String, code: String, f: Database => Unit): Unit = {
    test(name) {
      f(computeSemanticdbFromCode(code))
    }
  }

  implicit class NormalizedSyntheticCount(what: Seq[SyntheticCount]) {
    def normalized = what.map(_.copy(file = ""))
  }
}

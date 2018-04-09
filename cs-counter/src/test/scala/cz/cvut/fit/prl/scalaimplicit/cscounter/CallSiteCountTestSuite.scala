package cz.cvut.fit.prl.scalaimplicit.cscounter

import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import org.langmeta.semanticdb.Database

class CallSiteCountTestSuite extends SemanticdbTest {

  def checkDB(name: String, code: String, f: Database => Unit): Unit = {
    test(name) {
      f(computeSemanticdbFromCode(code))
    }
  }

  implicit class NormalizedCallSiteCount(what: Seq[CallSiteCount]) {
    def normalized = what.map(_.copy(file = ""))
  }
}

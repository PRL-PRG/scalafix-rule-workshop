package implicits

import fix.BuildInfo

import scala.meta._
import scalafix._
import scalafix.testkit._

class ScalafixExperimentsTest
  extends SemanticRuleSuite(
    SemanticdbIndex.load(Classpath(AbsolutePath(BuildInfo.inputClassdirectory))),
    AbsolutePath(BuildInfo.inputSourceroot),
    Seq(AbsolutePath(BuildInfo.outputSourceroot))
  ) {
  runAllTests()
}

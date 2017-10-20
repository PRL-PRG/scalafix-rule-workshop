package implicits

import fix.BuildInfo
import org.scalatest._

import scala.meta._
import scalafix._
import scalafix.testkit._

class ScalafixExperimentsTest
  extends SemanticRuleSuite(
          SemanticdbIndex.load(Classpath(AbsolutePath(BuildInfo.inputClassdirectory))),
          AbsolutePath(BuildInfo.inputSourceroot),
          Seq(AbsolutePath(BuildInfo.outputSourceroot))
          ) {

  private val suites = Map[String, CustomRuleSuite](
    "scala/implicits/SyntheticCallChains.scala" -> new SyntheticCallChainsTests
  )

  testsToRun.foreach {t =>
    println(t.name)
    runOn(t)
  }

  override def runOn(t: DiffTest): Unit = {
    val suite = suites.getOrElse(t.name, NotFoundTests)
    suite.run(t)
  }
}

abstract class CustomRuleSuite extends FunSuite {
  def run(t: DiffTest): Unit
}

object NotFoundTests extends CustomRuleSuite {
  def run(t: DiffTest): Unit = {
    println("Not found!")
  }
}


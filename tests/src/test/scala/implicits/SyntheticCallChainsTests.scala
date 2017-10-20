package implicits

import scalafix.testkit.DiffTest

class SyntheticCallChainsTests extends CustomRuleSuite {
  def run(t: DiffTest): Unit = {
    assert(t.document.synthetics.isEmpty)
  }
}
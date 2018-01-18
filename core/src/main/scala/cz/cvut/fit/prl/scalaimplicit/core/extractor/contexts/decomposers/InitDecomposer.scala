package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.decomposers

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts.{
  BreakDown,
  QualifiedSymbol
}

import scala.meta.{Init, Tree}

object InitDecomposer {
  def apply(init: Init, finder: Tree => QualifiedSymbol): BreakDown =
    BreakDown(
      symbol = finder(init.name),
      targs = Seq(init.tpe), //TODO not entirely sure about this, it will appear like [fun[Targs]]
      args = TermDecomposer.processParamList(init.argss.flatten)(finder),
      pos = init.pos
    )
}

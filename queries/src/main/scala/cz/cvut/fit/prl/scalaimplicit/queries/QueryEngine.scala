package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite

object QueryEngine {
  def apply(q: (CallSite => Boolean), res: ExtractionResult): Seq[CallSite] =
    res.callSites.filter(cs => q.apply(cs))
}

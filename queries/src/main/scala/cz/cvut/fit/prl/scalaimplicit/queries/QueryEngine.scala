package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ProjectMetadata,
  ProjectReport
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite

object QueryEngine {
  def apply(q: (CallSite => Boolean),
            data: Seq[ProjectReport]): Seq[ProjectReport] =
    data.map(
      proj =>
        proj.copy(
          result = proj.result.copy(
            callSites = proj.result.callSites.filter(cs => q(cs))
          )
      ))
}

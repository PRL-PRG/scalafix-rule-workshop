package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ProjectMetadata,
  ProjectReport,
  Statistics
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite

object QueryEngine {
  def apply(q: (CallSite => Boolean),
            data: Seq[ProjectReport]): Seq[ProjectReport] =
    data.map(
      proj => {
        val filteredCallSites = proj.result.callSites.filter(cs => q(cs))
        proj.copy(
          result = proj.result.copy(
            callSites = filteredCallSites
          ),
          stats = Statistics(
            percentageCovered = filteredCallSites.size.toDouble / proj.result.callSites.size.toDouble,
            callSitesAfterFilter = filteredCallSites.size,
            callSitesBeforeFilter = proj.result.callSites.size
          )
        )
      }
    )

  def contains[A](who: Iterable[A], filter: (A => Boolean)): Boolean =
    who match {
      case head :: tail => filter(head) || contains(tail, filter)
      case Nil => false
    }

  def matches[A](who: A, filter: (A => Boolean)): Boolean = filter(who)
}

package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ProjectMetadata,
  ProjectReport,
  Statistics
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite

object QueryEngine {
  def apply(
      q: (CallSite => Boolean),
      data: Seq[ProjectReport]): (Seq[ProjectReport], Seq[ProjectReport]) =
    data
      .map(
        proj => {
          val partition = proj.result.callSites.partition(cs => q(cs))
          createReportFromPartition(proj, partition._1) ->
            createReportFromPartition(proj, partition._2)
        }
      )
      .unzip

  def contains[A](who: Iterable[A], filter: (A => Boolean)): Boolean =
    who match {
      case head :: tail => filter(head) || contains(tail, filter)
      case Nil => false
    }

  def matches[A](who: A, filter: (A => Boolean)): Boolean = filter(who)

  private def createReportFromPartition(original: ProjectReport,
                                        css: Seq[CallSite]) =
    original.copy(
      result = original.result.copy(
        callSites = css
      ),
      stats = Statistics(
        percentageCovered = css.size.toDouble / original.result.callSites.size.toDouble,
        callSitesAfterFilter = css.size,
        callSitesBeforeFilter = original.result.callSites.size
      )
    )
}

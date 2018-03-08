package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  CallSite,
  Location
}
import cz.cvut.fit.prl.scalaimplicit.core.reports.{
  ProjectMetadata,
  ProjectReport,
  Statistics
}

trait FilterQuery[A] {
  def name: String
  def predicate: (A => Boolean)
}

object FilterQuery {

  case class CSFilterQuery(name: String, predicate: CallSite => Boolean)
      extends FilterQuery[CallSite]

  type LocationParams = (CallSite, ProjectMetadata)

  case class PathFilterQuery(name: String,
                             predicate: LocationParams => Boolean)
      extends FilterQuery[LocationParams]

}

object QueryEngine {
  def apply(
      q: FilterQuery.CSFilterQuery,
      data: Seq[ProjectReport]): (Seq[ProjectReport], Seq[ProjectReport]) =
    data
      .map(
        proj => {
          val partition =
            proj.result.callSites.partition(cs => q.predicate(cs))
          createReportFromPartition(proj, partition._1) ->
            createReportFromPartition(proj, partition._2)
        }
      )
      .unzip

  def apply(
      q: FilterQuery.PathFilterQuery,
      data: Seq[ProjectReport]): (Seq[ProjectReport], Seq[ProjectReport]) =
    data
      .map(
        proj => {
          val partition =
            proj.result.callSites
              .map(cs => cs -> proj.metadata)
              .partition(thing => q.predicate(thing))

          createReportFromPartition(proj, partition._1.map(_._1)) ->
            createReportFromPartition(proj, partition._2.map(_._1))
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

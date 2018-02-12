package cz.cvut.fit.prl.scalaimplicit.core.reports

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.SlimRepresentation.{
  SlimCallSite,
  SlimDefinition
}

trait Mergeable[T] {
  def name: String
  def merge(other: T): T
}

case class CallSiteOccurrences(name: String,
                               occurrences: Int,
                               isTransitive: Boolean)
    extends Mergeable[CallSiteOccurrences] {
  def merge(other: CallSiteOccurrences): CallSiteOccurrences = {
    assert(name == other.name)
    // We trust that we are conscious of when we are losing information
    copy(isTransitive = isTransitive && other.isTransitive,
         occurrences = occurrences + other.occurrences)
  }
}
object CallSiteOccurrences {
  def apply(cs: SlimCallSite): CallSiteOccurrences =
    CallSiteOccurrences(cs.name, 1, cs.declaration.location.isDefined)
}
case class DefinitionOccurrences(name: String, occurrences: Int)
    extends Mergeable[DefinitionOccurrences] {
  def merge(other: DefinitionOccurrences): DefinitionOccurrences = {
    assert(name == other.name)
    copy(occurrences = occurrences + other.occurrences)
  }
}
object DefinitionOccurrences {
  def apply(d: SlimDefinition): DefinitionOccurrences =
    DefinitionOccurrences(d.name, 1)
}
case class ReportSummary(
    reponame: String,
    callSites: Seq[CallSiteOccurrences],
    totalCallSites: Int,
    definitions: Seq[DefinitionOccurrences],
    totalDefinitions: Int,
    stats: Statistics
) {
  def sortedCallSites = callSites.sortBy(-_.occurrences)
  def sortedDefinitions = definitions.sortBy(-_.occurrences)
}
object ReportSummary {
  def apply(parts: Seq[ReportSummary]) = {
    def mergeList[T <: Mergeable[T]](maps: Seq[T]): Seq[T] =
      maps
        .groupBy(_.name)
        .values
        .map(group => group.reduce((x, y) => x.merge(y)))
        .toSeq

    val merge = (report: ReportSummary, acc: ReportSummary) =>
      acc.copy(
        callSites = mergeList(report.callSites ++ acc.callSites),
        totalCallSites = acc.totalCallSites + report.totalCallSites,
        definitions = mergeList(acc.definitions ++ report.definitions),
        totalDefinitions = acc.totalDefinitions + report.totalDefinitions,
        reponame = "all/summary"
    )

    if (parts.size == 1) parts.head.copy(reponame = "all/summary")
    else parts.tail.fold(parts.head)(merge)
  }

  def groupAndMerge[T <: Mergeable[T]](a: Seq[T]): (Seq[T], Int) = {
    val groups = a.groupBy(_.name)
    groups.values
      .map(x => x.reduce(_ merge _))
      .toSeq ->
      groups.values.map(_.size).sum
  }

  def apply(report: SlimReport): ReportSummary = {

    val css = groupAndMerge(
      report.result.callSites.map(x => CallSiteOccurrences(x)))
    val decls = groupAndMerge(
      report.result.definitions.map(x => DefinitionOccurrences(x)).toSeq)

    ReportSummary(report.metadata.reponame,
                  css._1,
                  css._2,
                  decls._1,
                  decls._2,
                  report.stats)
  }

  def apply(defSum: DefinitionSummary) = {
    new ReportSummary(
      defSum.metadata.reponame,
      callSites = Seq(),
      totalCallSites = 0,
      definitions = groupAndMerge(
        defSum.definitions
          .map(x => DefinitionOccurrences(x._1, x._2))
          .toSeq)._1,
      totalDefinitions = defSum.definitions.values.sum,
      stats = Statistics.Default
    )
  }
}
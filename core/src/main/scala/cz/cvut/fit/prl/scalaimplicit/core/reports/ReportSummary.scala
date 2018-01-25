package cz.cvut.fit.prl.scalaimplicit.core.reports

case class ReportSummary(
    reponame: String,
    callSites: Map[String, Int],
    totalCallSites: Int,
    definitions: Map[String, Int],
    totalDefinitions: Int,
    stats: Statistics
) {
  def sortedCallSites = callSites.toSeq.sortBy(-_._2)
  def sortedDefinitions = definitions.toSeq.sortBy(-_._2)
}
object ReportSummary {
  def apply(parts: Seq[ReportSummary]) = {
    def mergeMaps(maps: Seq[Map[String, Int]]): Map[String, Int] =
      maps.reduceLeft((r, m) =>
        m.foldLeft(r) {
          case (dict, (k, v)) => dict + (k -> (v + dict.getOrElse(k, 0)))
      })

    val merge = (report: ReportSummary, acc: ReportSummary) =>
      acc.copy(
        callSites = mergeMaps(Seq(acc.callSites, report.callSites)),
        totalCallSites = acc.totalCallSites + report.totalCallSites,
        definitions = mergeMaps(Seq(acc.definitions, report.definitions)),
        totalDefinitions = acc.totalDefinitions + report.totalDefinitions,
        reponame = "all/summary"
    )

    if (parts.size == 1) parts.head.copy(reponame = "all/summary")
    else parts.tail.fold(parts.head)(merge)
  }

  def apply(report: SlimReport): ReportSummary = {
    def groupAndCount(
        what: Iterable[{ def name: String }]): (Map[String, Int], Int) = {
      val groups = what
        .groupBy(_.name)
        .map(x => (x._1, x._2.size))
      (groups, groups.values.sum)
    }

    val css = groupAndCount(report.result.callSites)
    val decls = groupAndCount(report.result.definitions)

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
      callSites = Map(),
      totalCallSites = 0,
      definitions = defSum.definitions.map(x => x._1 -> x._2),
      totalDefinitions = defSum.definitions.values.sum,
      stats = Statistics.Default
    )
  }
}

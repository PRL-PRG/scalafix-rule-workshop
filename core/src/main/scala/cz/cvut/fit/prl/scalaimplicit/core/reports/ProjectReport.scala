package cz.cvut.fit.prl.scalaimplicit.core.reports

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ImplicitAnalysisResult

case class ProjectReport(
    metadata: ProjectMetadata,
    result: ImplicitAnalysisResult
)
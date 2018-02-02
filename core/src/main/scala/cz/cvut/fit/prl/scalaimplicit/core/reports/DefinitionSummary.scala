package cz.cvut.fit.prl.scalaimplicit.core.reports

case class DefinitionSummary(metadata: ProjectMetadata,
                             definitions: Map[String, Int])

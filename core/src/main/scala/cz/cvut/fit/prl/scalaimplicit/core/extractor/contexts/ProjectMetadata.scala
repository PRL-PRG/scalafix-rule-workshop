package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

case class ProjectMetadata(
    reponame: String,
    name: String,
    url: String,
    lastCommit: String,
    buildSystem: String,
    version: String,
    ghStars: Int,
    totalLOC: Int,
    scalaLOC: Int
)

object ProjectMetadata {
  def loadFrom(path: String): ProjectMetadata = ???
}

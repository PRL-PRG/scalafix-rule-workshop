package cz.cvut.fit.prl.scalaimplicit.core.reports
import java.io.File

import com.github.tototoshi.csv._

import scala.collection.immutable

case class ProjectMetadata(
    reponame: String,
    name: String,
    url: String,
    lastCommit: String,
    buildSystem: String,
    version: String,
    ghStars: Int,
    totalLOC: Int,
    scalaLOC: Int,
    mainPaths: Seq[String],
    testPaths: Seq[String]
)

object ProjectMetadata {
  private def loadCSV(path: String): Seq[Map[String, String]] = {
    CSVReader
      .open(new File(path))
      .allWithHeaders()
      .map(_.map(x => x._1.trim -> x._2.trim))
  }

  def processPaths(
      rawdata: Seq[Map[String, String]]): Map[String, Seq[String]] = {
    def convertToRelativePaths(entry: Map[String, String]): (String, Seq[String]) = 
      entry("kind") -> {
        val split = entry("path").split(s"(?=/${entry("project")})").tail.map(_.stripPrefix("/"))
        split.tail.foldLeft(Seq(split.head))(((acc, elem) => acc.map(_ + "/" + elem) ++ Seq(elem)))
      }
   

    rawdata
      .map(convertToRelativePaths)
      .groupBy(_._1)
      .map(p => p._1 -> p._2.flatMap(_._2))
  }

  def loadFromCSV(metadataFile: String, pathsFile: String): ProjectMetadata = {
    val metadata = loadCSV(metadataFile).head
    val paths = processPaths(loadCSV(pathsFile))
    ProjectMetadata(
      reponame = metadata("reponame"),
      name = metadata("name"),
      url = metadata("url"),
      lastCommit = metadata("last_commit"),
      buildSystem = metadata("build_system"),
      version = metadata("version"),
      ghStars = metadata("gh_stars").toInt,
      totalLOC = metadata("total_loc").toInt,
      scalaLOC = metadata("scala_loc").toInt,
      mainPaths = paths.get("compile").getOrElse(Seq("/")), // If no main path is given, we assume it's all main
      testPaths = paths.get("test").getOrElse(Seq("nothing will match this"))
    )
  }
}

package extractor

import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.concurrent.ConcurrentLinkedQueue

object CSV {

  trait Serializable {
    def csvHeader: Seq[String]
    def csvValues: Seq[String]
    def id: String
  }

  def writeCSV(xs: Iterable[Serializable], path: String): Unit = {
    def prepareValue(x: String) = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    if (xs.nonEmpty) {
      val header = xs.head.csvHeader.mkString(",")
      val values =
        xs.map(_.csvValues.map(prepareValue).mkString(","))
          .mkString("\n") +
          "\n"

      if (Files.exists(Paths.get(path))) {
        Files.write(Paths.get(path), s"$values".getBytes, StandardOpenOption.APPEND)
      } else {
        Files.write(Paths.get(path), s"$header\n$values".getBytes)
      }
    }
  }


  def dumpFiles(projectPath: String, results: Map[String, Iterable[CSV.Serializable]]) = {
    results.foreach(x => {
      CSV.writeCSV(x._2, s"${projectPath}/${x._1}")
    })
  }
}

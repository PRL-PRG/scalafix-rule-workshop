package cz.cvut.fit.prl.scalaimplicit.core.extractor

import java.nio.file.{Files, Paths, StandardOpenOption}

object CSV {

  trait Serializable {
    def csvHeader: Seq[String]
    def csvValues: Seq[String]
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
        Files.write(Paths.get(path),
                    s"$values".getBytes,
                    StandardOpenOption.APPEND)
      } else {
        Files.write(Paths.get(path), s"$header\n$values".getBytes)
      }
    }
  }

  def dumpFiles(projectPath: String, results: Result) = {
    CSV.writeCSV(results.params, s"$projectPath/params.csv")
    CSV.writeCSV(results.funs, s"$projectPath/funs.csv")
    CSV.writeCSV(results.links, s"$projectPath/params-funs.csv")
    CSV.writeCSV(results.implicits, s"$projectPath/declared-implicits.csv")
  }
}

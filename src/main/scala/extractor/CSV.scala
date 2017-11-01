package extractor

import java.nio.file.{Files, Paths, StandardOpenOption}

object CSV {

  trait Serializable[A] {
    def csvHeader: Seq[String]
    def csvValues: Seq[String]
    def id: String
  }

  def writeCSV[A](xs: Iterable[_ <: Serializable[A]], path: String): Unit = {
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
}

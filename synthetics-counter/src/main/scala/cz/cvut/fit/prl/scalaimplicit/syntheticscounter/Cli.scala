package cz.cvut.fit.prl.scalaimplicit.syntheticscounter

import java.nio.file.{Files, Paths}

case class CliConfig(root: String = "Nodir", outdir: String = "Nodir")
object Cli {
  def apply(args: Array[String]): Option[CliConfig] = {
    val optParser = new scopt.OptionParser[CliConfig]("collector") {
      head("PRL-PRG Synthetic Counter (c)", "0.1")
      arg[String]("<input folder>")
        .validate(
          x =>
            if (Files.exists(Paths.get(x)) && Files.isDirectory(Paths.get(x)))
              success
            else failure("Folder does not exist or is not a folder"))
        .action((x, c) => c.copy(root = x))
        .text("The directory where the project(s) is stored")
      arg[String]("<output folder>")
        .validate(
          x =>
            if (Files.exists(Paths.get(x)) && Files.isDirectory(Paths.get(x)))
              success
            else failure("Out folder does not exist or is not a directory")
        )
        .action((x, c) => c.copy(outdir = x))
        .text("The desired place to output the files")
    }

    optParser.parse(args, CliConfig())
  }

}

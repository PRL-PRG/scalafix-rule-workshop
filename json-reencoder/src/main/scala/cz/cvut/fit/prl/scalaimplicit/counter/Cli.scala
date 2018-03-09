package cz.cvut.fit.prl.scalaimplicit.counter

import java.nio.file.{Paths, Files}

case class CliConfig(source: String = "Nodir", outdir: String = "Nodir")
object Cli {
  def apply(args: Array[String]): Option[CliConfig] = {
    val optParser = new scopt.OptionParser[CliConfig]("reencoder") {
      head("PRL-PRG Call Json Reencoder (c)", "0.1")
      arg[String]("<input>")
        .validate(
          x =>
            if (Files.exists(Paths.get(x)))
              success
            else failure("Input file does not exist"))
        .action((x, c) => c.copy(source = x))
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

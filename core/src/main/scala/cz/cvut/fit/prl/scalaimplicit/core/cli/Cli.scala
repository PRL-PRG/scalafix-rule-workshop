package cz.cvut.fit.prl.scalaimplicit.core.cli

import java.nio.file.{Paths, Files}

case class CliConfig(root: String = "Nodir", classpath: String = "Nocp", outdir: String = "./tmp")
object Cli {
  def apply(args: Array[String]): Option[CliConfig] = {
    val optParser = new scopt.OptionParser[CliConfig]("collector") {
      head("PRL-PRG Implicit Collector (c)", "0.1")
      arg[String]("<directory>")
        .validate(
          x =>
            if (Files.exists(Paths.get(x)) && Files.isDirectory(Paths.get(x)))
              success
            else failure("Folder does not exist or is not a folder"))
        .action((x, c) => c.copy(root = x))
        .text("The directory where the project(s) is stored")
      arg[String]("<stored classpath>")
        .validate(
          x =>
            if (Files.exists(Paths.get(x)))
              success
            else failure("Classpath not found"))
        .action((x, c) => c.copy(classpath = x))
        .text("The stored classpath from the plugin")
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

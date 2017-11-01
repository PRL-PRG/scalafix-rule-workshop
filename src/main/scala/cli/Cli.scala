package cli

import scopt._
import java.nio.file.{Paths, Files}

case class CliConfig(root: String = "Nodir", multidir: Boolean = false)
object Cli {    
    def apply(args: Array[String]): Option[CliConfig] = {
        val optParser = new scopt.OptionParser[CliConfig]("collector") {
            head("PRL-PRG Implicit Collector (c)", "0.1")
            arg[String]("<directory>")
                .validate(x => 
                    if (Files.exists(Paths.get(x)) && Files.isDirectory(Paths.get(x))) success
                    else failure("Folder does not exist or is not a folder"))
                .action( (x, c) => c.copy(root = x) )
                .text("The directory where the project(s) is stored")
            opt[Unit]('m', "multiple")
                .optional()
                .action((_, c) => c.copy(multidir = true))
                .text("Use if multiple projects are in the folder")
        }

        optParser.parse(args, CliConfig())
    }
    
}
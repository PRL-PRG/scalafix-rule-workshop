import java.io.{File, UncheckedIOException}
import java.net.URLClassLoader
import java.nio.file.{AccessDeniedException, Files}

import extractor.ImplicitParamsToCSV
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.semanticdb.Database
import org.scalatest.{FunSuite, Matchers}

import scala.compat.Platform.EOL
import scala.meta.internal.semanticdb.{DatabaseOps, FailureMode, ProfilingMode, SemanticdbMode}
import scala.meta.io.AbsolutePath
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.util.{Failure, Success, Try}


abstract class SemanticdbTest extends FunSuite with Matchers {
  private val workingDir = File.createTempFile("semanticdb-test", "").getParentFile

  private lazy val g: Global = {
    val semanticdbPluginPath: String = {
      val semanticdbPluginPathProperty = "semanticdb-scalac-jar"

      val candidate = Option(System.getProperty(semanticdbPluginPathProperty, null)).getOrElse(Try {
        // this is ugly for it is just for our tests, we can live with that
        val cl = classOf[org.langmeta.semanticdb.Database].getClassLoader.asInstanceOf[URLClassLoader]
        cl.getURLs
          .map(_.getFile)
          .filter(_.matches(".*/semanticdb-scalac[^jar]*jar$"))
          .head
      } match {
        case Success(file) => file
        case Failure(e) =>
          e.printStackTrace(System.err)

          fail("unable to figure out the path of semanticdb-scalac.jar. " +
            s"Please set it up manually using -D$semanticdbPluginPathProperty", e)
      })

      assert(new File(candidate).exists())

      candidate
    }

    val options = s"-Xplugin:$semanticdbPluginPath -Yrangepos -Xplugin-require:semanticdb"
    val args = CommandLineParser.tokenize(options)

    val emptySettings = new Settings(error => fail(s"couldn't apply settings because $error"))
    emptySettings.usejavacp.value = true
    emptySettings.outputDirs.setSingleOutput(workingDir.getCanonicalPath)

    val command = new CompilerCommand(args, emptySettings)
    new Global(command.settings)
  }

  private lazy val databaseOps: DatabaseOps = new DatabaseOps {
    val global: Global = SemanticdbTest.this.g
  }

  import databaseOps._

  config.setMode(SemanticdbMode.Slim)
  config.setFailures(FailureMode.Error)
  config.setProfiling(ProfilingMode.Console)

  //  databaseOps.config.setSourceroot(AbsolutePath(workingDir))
  config.setSourceroot(AbsolutePath(workingDir))

  def computeSemanticdbFromCode(code: String): Database = {
    val testFile = File.createTempFile("semanticdb-test-", ".scala")
    val semanticdbFile = new File(workingDir, testFile.getName.replaceFirst("\\.scala$", ".semanticdb"))

    // we have set up the compiler to use the working dir
    // if this does not hold, we won't find the semanticsdb file
    assert(testFile.getParentFile.getCanonicalPath == workingDir.getCanonicalPath)

    Files.write(testFile.toPath, code.getBytes)

    try {
      new g.Run().compile(List(testFile.getCanonicalPath))
    } catch {
      case e: UncheckedIOException if e.getCause.isInstanceOf[AccessDeniedException] && semanticdbFile.exists() =>
        // there is a bug in scalameta trying to access files it should not
        // ignore it if it generated file
      case e: Throwable => fail(s"Unable to compile", e)
    } finally {
      assert(testFile.delete())
    }

    if (!semanticdbFile.exists()) {
      fail(s"Unable to find semanticdb file - expected at `${semanticdbFile.getCanonicalPath}'")
    }

    val sdb = s.Database.parseFrom(Files.readAllBytes(semanticdbFile.toPath))
    sdb.toDb(None)
  }

  private def test(code: String)(fn: => Unit): Unit = {
    var name = code.trim.replace(EOL, " ")
    if (name.length > 50) name = name.take(50) + "..."
    super.test(name)(fn)
  }

  protected def checkExtraction(code: String, f: ImplicitParamsToCSV.Result => Unit): Unit = {
    test(code) {
      val db = computeSemanticdbFromCode(code)
      val result = ImplicitParamsToCSV.process(db)

      f(result)
    }
  }

}

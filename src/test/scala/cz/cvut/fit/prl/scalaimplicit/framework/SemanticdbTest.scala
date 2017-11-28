package cz.cvut.fit.prl.scalaimplicit.framework

import java.io.{File, UncheckedIOException}
import java.net.URLClassLoader
import java.nio.file.{AccessDeniedException, Files}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.extractor.Serializables.{
  Apply,
  DeclaredImplicit
}
import cz.cvut.fit.prl.scalaimplicit.extractor.{
  ExtractImplicits,
  Location,
  Result,
  SemanticCtx
}
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.semanticdb.Database
import org.scalatest.{FunSuite, Matchers}

import scala.compat.Platform.EOL
import scala.meta.internal.semanticdb.{
  DatabaseOps,
  FailureMode,
  ProfilingMode,
  SemanticdbMode
}
import scala.meta.io.AbsolutePath
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.util.{Failure, Success, Try}

abstract class SemanticdbTest extends FunSuite with Matchers with LazyLogging {
  private val workingDir =
    File.createTempFile("semanticdb-test", "").getParentFile

  private def findJar(pattern: String): Try[String] = Try {
    // this is ugly for it is just for our tests, we can live with that
    val cl = classOf[org.langmeta.semanticdb.Database].getClassLoader
      .asInstanceOf[URLClassLoader]
    cl.getURLs
      .map(_.getFile)
      .filter(_.matches(pattern))
      .head
  }

  private lazy val g: Global = {
    val semanticdbPluginPath: String = {
      val propertyName = "semanticdb-scalac-jar"

      val candidate = Option(System.getProperty(propertyName, null))
        .getOrElse {
          findJar(".*/semanticdb-scalac[^jar]*jar$") match {
            case Success(file) => file
            case Failure(e) =>
              e.printStackTrace(System.err)

              fail("unable to figure out the path of semanticdb-scalac.jar. " +
                     s"Please set it up manually using -D$propertyName",
                   e)
          }
        }

      assert(new File(candidate).exists())

      candidate
    }

    val options =
      s"-Xplugin:$semanticdbPluginPath -Yrangepos -Xplugin-require:semanticdb"
    val args = CommandLineParser.tokenize(options)

    val emptySettings = new Settings(
      error => fail(s"couldn't apply settings because $error"))
    emptySettings.outputDirs.setSingleOutput(workingDir.getCanonicalPath)

    // trying to figure out if we run from intellij or from sbt
    // FIXME: is there any smarter way to do this?
    findJar(".*/scala-plugin-runners\\.jar$") match {
      case Success(_) => emptySettings.usejavacp.value = true // intellij
      case Failure(_) => emptySettings.embeddedDefaults[SemanticdbTest] // sbt
    }

    val command = new CompilerCommand(args, emptySettings)
    val reporter = new ConsoleReporter(command.settings)

    new Global(command.settings, reporter)
  }

  private lazy val databaseOps: DatabaseOps = new DatabaseOps {
    val global: Global = SemanticdbTest.this.g
  }

  import databaseOps._

  config.setMode(SemanticdbMode.Slim)
  config.setFailures(FailureMode.Error)
  config.setProfiling(ProfilingMode.Console)
  config.setSourceroot(AbsolutePath(workingDir))

  def computeSemanticdbFromCode(code: String): Database = {
    val testFile = File.createTempFile("semanticdb-test-", ".scala")
    val semanticdbFile = new File(
      workingDir,
      testFile.getName.replaceFirst("\\.scala$", ".semanticdb"))

    // we have set up the compiler to use the working dir
    // if this does not hold, we won't find the semanticsdb file
    assert(
      testFile.getParentFile.getCanonicalPath == workingDir.getCanonicalPath)

    Files.write(testFile.toPath, code.getBytes)

    try {
      //logger.debug(s"Compiling $testFile with "+g.settings.toString())

      new g.Run().compile(List(testFile.getCanonicalPath))
    } catch {
      case e: UncheckedIOException
          if e.getCause
            .isInstanceOf[AccessDeniedException] && semanticdbFile.exists() =>
      // there is a bug in scalameta trying to access files it should not
      // ignore it if it generated file
      case e: Throwable => fail(s"Unable to compile", e)
    } finally {
      assert(testFile.delete())
    }

    if (!semanticdbFile.exists()) {
      fail(
        s"Unable to find semanticdb file - expected at `${semanticdbFile.getCanonicalPath}'")
    }

    val sdb = s.Database.parseFrom(Files.readAllBytes(semanticdbFile.toPath))
    sdb.toDb(None)
  }

  private def test(code: String)(fn: => Unit): Unit = {
    var name = code.trim.replace(EOL, " ")
    if (name.length > 50) name = name.take(50) + "..."
    super.test(name)(fn)
  }

  protected def checkContext(name: String,
                             code: String,
                             f: SemanticCtx => Unit): Unit = {
    test(name) {
      val db = computeSemanticdbFromCode(code)
      val ctx = SemanticCtx(db)

      f(ctx)
    }
  }

  protected def checkExtraction(name: String,
                                code: String,
                                f: Result => Unit): Unit = {
    test(name) {
      val db = computeSemanticdbFromCode(code)
      val ctx = SemanticCtx(db)
      val result = ExtractImplicits(ctx)

      f(result)
    }
  }
  implicit class NormalizedResult(that: Result) {
    def normalizedImplicits: Set[DeclaredImplicit] =
      that.implicits.map(_.copy(location = Location.Empty))
    // Note that when using normalized funs we cannot make assertions over the links,
    // Because the links are tied to the position of the application.
    def normalizedFuns: Seq[Apply] =
      that.funs.map(_.copy(location = Location.Empty))
  }

}

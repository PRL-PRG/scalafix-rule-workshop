package cz.cvut.fit.prl.scalaimplicit.core.framework

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.core.extractor._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters.PrettyInstances.PrettyCallSite
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.semanticdb.Database
import org.scalatest.exceptions.TestFailedException
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
import java.io.{File, UncheckedIOException}
import java.net.URLClassLoader
import java.nio.file.{AccessDeniedException, Files}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  Location => _,
  _
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.{
  JSONSerializer,
  PrettyPrinters
}

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

  protected def checkReflContext(name: String,
                                 code: String,
                                 f: ReflectiveCtx => Unit): Unit = {
    test(name) {
      // FIXME Perhaps computesemanticdbFromCode could return the path of the compiled sources
      val db = computeSemanticdbFromCode(code)
      val loader =
        new URLClassLoader(Array(workingDir.getCanonicalFile.toURI.toURL),
                           this.getClass.getClassLoader)
      val ctx = new ReflectiveCtx(loader, db)

      f(ctx)
    }
  }

  implicit class NormalizedRes(that: ExtractionResult) {
    def normalized = that.copy(
      callSites = normalizedCallSites,
      declarations = normalizedDeclarations
    )

    def onlyCallSites = that.copy(
      callSites = that.callSites,
      declarations = Set()
    )

    def onlyDeclarations = that.copy(
      callSites = Seq(),
      declarations = that.declarations
    )

    def onlyImplicitDeclarations = that.copy(
      callSites = Seq(),
      declarations = that.declarations.filter(_.isImplicit)
    )

    def normalizedCallSites = {
      that.callSites
        .map(
          x =>
            x.copy(
              location = normalizedLocation(x.location),
              declaration = normalizedDeclaration(x.declaration),
              implicitArguments = x.implicitArguments.map(normalizedArgument)
          ))
    }

    def sortedCallSites: Seq[CallSite] =
      that.callSites.sortBy(_.location.get.line)

    def sortedDeclarations: Seq[Declaration] =
      that.declarations.toSeq.sortBy(_.location.get.line)

    def normalizedDeclarations: Set[Declaration] =
      that.declarations.map(normalizedDeclaration)

    private def normalizedDeclaration(d: Declaration): Declaration = d.copy(
      location = normalizedLocation(d.location),
      parents = d.parents.map(normalizedParent)
    )

    private def normalizedParent(p: Parent) = p.copy(
      declaration = normalizedDeclaration(p.declaration)
    )

    private def normalizedArgument(arg: ArgumentLike): ArgumentLike =
      arg match {
        case a: Argument => a
        case a: ImplicitArgument =>
          a.copy(
            declaration = normalizedDeclaration(a.declaration),
            arguments = a.arguments.map(normalizedArgument)
          )
      }
    private def normalizedLocation(location: Option[Representation.Location])
      : Option[Representation.Location] =
      location.map(loc => loc.copy(file = ""))
  }

  /**
    * Copied from scala.meta.testkit.DiffAssertions.
    */
  def compareContents(original: Seq[String], revised: Seq[String]): String = {
    import collection.JavaConverters._
    def trim(lines: Seq[String]) = lines.map(_.trim).asJava
    val diff =
      difflib.DiffUtils.diff(trim(lines(original)), trim(lines(revised)))
    if (diff.getDeltas.isEmpty) ""
    else
      difflib.DiffUtils
        .generateUnifiedDiff(
          "original",
          "revised",
          original.asJava,
          diff,
          1
        )
        .asScala
        .drop(3)
        .mkString("\n")
  }

  /**
    * Get all the lines of the results.
    * We sort by length just so that all the callsites appear in the same order
    * in both result and expected values
    */
  def lines(content: Seq[String]): Seq[String] =
    content.sortBy(_.length).flatMap(_.split("\n").map(_.trim))

  protected def checkPrettyReflRes(name: String,
                                   code: String,
                                   expected: Seq[String]): Unit = {

    test(name) {
      val db = computeSemanticdbFromCode(code)
      val loader =
        new URLClassLoader(Array(workingDir.getCanonicalFile.toURI.toURL),
                           this.getClass.getClassLoader)
      val ctx = new ReflectiveCtx(loader, db)
      val res = ReflectExtract(ctx)
      val resStrings: Seq[String] =
        res.normalizedCallSites.map(
          PrettyPrinters.prettyPrint(_)(PrettyCallSite).trim)
      compareContents(lines(resStrings), lines(expected)) shouldBe empty
    }
  }

  /*
  Return the diff between the prettified JSONs of two results
   */
  protected def compareJSON(one: ExtractionResult,
                            other: ExtractionResult): String = {
    def JSONLines(from: ExtractionResult): Seq[String] = {
      lines(Seq(JSONSerializer.prettyJSON(from)))
    }
    compareContents(JSONLines(one), JSONLines(other))
  }
}

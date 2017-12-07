package cz.cvut.fit.prl.scalaimplicit.extractor

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.prl.scalaimplicit.extractor.Representation.Declaration
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb.{schema => s}

import scala.meta._
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.util.control.NonFatal

case class SemanticCtx(database: Database) extends LazyLogging {
  def input = database.documents.head.input
  val file: String = input match {
    case Input.VirtualFile(path, _) => path
    case Input.File(path, _)        => path.toString
    case _                          => ""
  }

  private lazy val _denots: Map[Symbol, Denotation] = {
    val builder = Map.newBuilder[Symbol, Denotation]
    database.symbols.foreach(r => builder += (r.symbol -> r.denotation))
    builder.result()
  }
  private lazy val _names: Map[Position, ResolvedName] = {
    val builder = Map.newBuilder[Position, ResolvedName]
    def add(r: ResolvedName) = {
      builder += (r.position -> r)
    }
    database.documents.foreach { entry =>
      entry.names.foreach(add)
      entry.synthetics.foreach(_.names.foreach(add))
      entry.symbols.foreach(_.denotation.names.foreach(add))
    }
    builder.result()
  }
  def symbol(position: Position): Option[Symbol] =
    _names.get(position).map(_.symbol)
  def symbol(tree: Tree): Option[Symbol] = tree match {
    case name @ Name(_) =>
      val syntax = name.syntax
      // workaround for https://github.com/scalameta/scalameta/issues/1083
      val pos =
        if (syntax.startsWith("(") &&
            syntax.endsWith(")") &&
            syntax != name.value)
          Position.Range(name.pos.input, name.pos.start + 1, name.pos.end - 1)
        else name.pos
      symbol(pos)
    case Importee.Rename(name, _)       => symbol(name)
    case Importee.Name(name)            => symbol(name)
    case Term.Select(_, name @ Name(_)) => symbol(name)
    case Type.Select(_, name @ Name(_)) => symbol(name)
    case _                              => symbol(tree.pos)
  }
  def denotation(symbol: Symbol): Option[Denotation] =
    _denots.get(symbol)
  def denotation(tree: Tree): Option[Denotation] =
    symbol(tree).flatMap(denotation)
  def names: Seq[ResolvedName] = _names.values.toSeq

  def tree: Source = input.parse[Source].get
  implicit val index = database.documents.head

  def qualifiedName(term: Term): String = {
    term match {
      case fun: Term.Name => {
        s"${symbol(fun).getOrElse(s"<unknown fun: ${fun}>")}"
      }
      case fun: Term.Select => {
        s"${symbol(fun.name).getOrElse(qualifiedName(fun.name))}"
      }
      case fun: Term.ApplyType => {
        qualifiedName(fun.fun)
      }
      case fun: Term.Apply => {
        symbol(fun).getOrElse(qualifiedName(fun.fun)).toString
      }
      case fun: Term.ApplyInfix => {
        symbol(fun).getOrElse(qualifiedName(fun.op)).toString
      }
      case other => {
        Console.withOut(Console.err) {
          logger.debug(s"[error] Function type unknown: ${other.structure}")
        }
        throw new RuntimeException()
      }
    }
  }

  def getKind(denot: Denotation): String = {
    var kind: String = denot match {
      case x if x.isVal                => "val"
      case x if x.isVar                => "var"
      case x if x.isDef                => "def"
      case x if x.isTrait              => "trait"
      case x if x.isMacro              => "macro"
      case x if x.isObject             => "object"
      case x if x.isVal && x.isLazy    => "lazy val"
      case x if x.isClass && !x.isCase => "class"
      case x if x.isClass && x.isCase  => "case class"
      case x if x.isParam              => "param"
      case x if x.isPackage            => "package"
      case x if x.isPackageObject      => "package object"
      case x                           => s"<unknown: ${x.structure}>"
    }
    //if (denot.isImplicit) kind = s"implicit $kind"
    if (denot.isFinal) kind = s"final $kind"
    if (denot.isLazy) kind = s"lazy $kind"
    if (denot.isAbstract) kind = s"abstract $kind"
    kind
  }

  def getTypeKind(denot: Denotation): String = {
    denot match {
      case den => {
        var kind: String = den match {
          case x if x.isClass && x.isCase  => "case class"
          case x if x.isClass && !x.isCase => "class"
          case x if x.isObject             => "object"
          case x if x.isTrait              => "trait"
          case _                           => ""
        }
        if (den.isImplicit) kind = s"implicit $kind"
        if (den.isFinal) kind = s"final $kind"
        if (den.isLazy) kind = s"lazy $kind"
        kind
      }
    }
  }
}

class ReflectiveCtx(loader: ClassLoader, db: Database) extends SemanticCtx(db) {
  import scala.reflect.runtime.{universe => u}
  val _mirror = u.runtimeMirror(loader)

  /**
    * Fetch the possible scala.reflect representations of a scala.meta Symbol
    * @param symbol The scala.meta.Symbol whose representation we want
    * @return A set of possible symbols with the same name as `symbol`
    */
  def fetchReflectSymbol(symbol: Symbol): Set[u.Symbol] = {

    /**
      * Extract a scala-reflect compatible fqn from a symbol.
      * `searchWoleSymbol` will be true when we need to
      * statically load the whole symbol ''in addition'' to
      * searching in the members of the owner
      * @param symbol
      * @return
      */
    case class ReflectLoadable(owner: String,
                               term: String,
                               searchWholeSymbol: Boolean)
    def splitFqn(symbol: Symbol): ReflectLoadable = {
      val owner =
        symbol
          .productElement(0)
          .toString
          .stripPrefix("_empty_.")
          .stripPrefix("_root_.")
          .stripSuffix(".")
          .stripSuffix("#")
      val name =
        symbol
          .productElement(1)
          .toString
          .split("""\(""")
          .head
      val isType = name.endsWith("#") // In scalameta, symbols that end in # are type names
      val tryWhole = isType
      ReflectLoadable(owner,
                      name
                        .stripSuffix("#")
                        .stripSuffix("."),
                      tryWhole)
    }

    /**
      * Given an owner and a term name to search, search for it with the mirror.
      * scala.reflect does not have a way to tell whether an fqn is a package, module or class.
      * Therefore, we have to try to load a symbol as each of those. https://goo.gl/MzBoJF
      *
      * It also may be that `name` is not there when we load something as a class, but
      * it appears when whe load it as a module (or vice versa).
      * Therefore, we have to keep looking if we don't find it at first.
      * FIXME: Exception-based flow control is obviously bad, we should look for a better way.
      */
    object SymbolSearch extends (ReflectLoadable => Set[u.Symbol]) {
      def apply(loadable: ReflectLoadable): Set[u.Symbol] = {
        val candidates = (classMembers(loadable.owner) ++
          moduleMembers(loadable.owner) ++
          packageMembers(loadable.owner))
          .filter(_.name.toString == loadable.term)
          .toSet
        if (loadable.searchWholeSymbol)
          candidates ++ getSymbol(s"${loadable.owner}.${loadable.term}")
        else candidates
      }

      // Get a single symbol from an fqn
      private def getSymbol(fqn: String): Set[u.Symbol] = {
        val s =
          loadClass(fqn).getOrElse(
            loadModule(fqn).getOrElse(loadPackage(fqn).getOrElse(u.NoSymbol)))
        if (s == u.NoSymbol) Set()
        else Set(s)
      }

      def loadClass(symbol: String): Option[u.ClassSymbol] = {
        try {
          Some(_mirror.staticClass(symbol))
        } catch {
          case _: ScalaReflectionException => None
        }
      }
      def loadModule(symbol: String): Option[u.ModuleSymbol] = {
        try {
          Some(_mirror.staticModule(symbol))
        } catch {
          case _: ScalaReflectionException => None
        }
      }
      def loadPackage(symbol: String): Option[u.ModuleSymbol] = {
        try {
          Some(_mirror.staticPackage(symbol))
        } catch {
          // FIXME This is an umbrella to catch both ScalaReflectionException and java ReflectionError
          case _: Throwable => None
        }
      }

      def classMembers(symbol: String): Seq[u.Symbol] = {
        loadClass(symbol).getOrElse(u.NoSymbol).typeSignature.members.toSeq
      }
      def moduleMembers(symbol: String): Seq[u.Symbol] = {
        loadModule(symbol) match {
          case Some(s) => s.moduleClass.info.members.toSeq
          case None    => Seq()
        }
      }
      def packageMembers(symbol: String): Seq[u.Symbol] = {
        loadPackage(symbol) match {
          case Some(s) => s.moduleClass.info.members.toSeq
          case None    => Seq()
        }
      }
    }

    val loadable = splitFqn(symbol)
    val candidates = SymbolSearch(loadable)
    assert(candidates.nonEmpty,
           s"We were unable to find anything matching $loadable")
    candidates
  }
}

object SemanticDBFileVisitor
    extends ((Path, (SemanticCtx => Result)) => Result)
    with LazyLogging {
  def apply(filePath: Path, f: SemanticCtx => Result): Result = {
    try {
      val sdb = s.Database.parseFrom(Files.readAllBytes(filePath))
      val mdb = sdb.toDb(None)
      val ctx = SemanticCtx(mdb)
      val res = f(ctx)
      logger.debug(s"Processing $filePath")
      res
    } catch {
      case NonFatal(e) =>
        val st = e.getStackTrace
        e.setStackTrace(st.take(10))
        e.printStackTrace()
        Result.Empty
    }
  }
}

trait TreeWalker extends ((SemanticCtx => Result) => Result) {
  def apply(f: SemanticCtx => Result): Result
  def deleteOldFiles(projectPath: AbsolutePath): Unit = {
    val files = Files
      .walk(projectPath.toNIO)
      .filter { file =>
        Files.isRegularFile(file) &&
        PathIO.extension(file) == "csv" &&
        file.getFileName.toString != "project.csv"
      }
    files.forEach { file =>
      Files.delete(file)
    }
  }

  /**
    * Function that, given two Results, it returns a Result that contains all the non-duplicate elements of both parameters.
    *
    * @param one
    * @param other
    * @return a Result that contains all the non-duplicate elements of one and other.
    */
  def mergeResults(one: Result, other: Result): Result = {
    def mergeById[A <: ResultElement](one: Set[A], other: Set[A]): Set[A] = {
      (one ++ other)
        .groupBy(_.id)
        .map { case (_, v) => v.head }
        .toSet
    }

    Result(
      mergeById(one.params, other.params),
      one.funs ++ other.funs,
      mergeById(one.links, other.links),
      mergeById(one.implicits, other.implicits)
    )
  }
}

class SingleProjectWalker(rootPath: String)
    extends TreeWalker
    with LazyLogging {
  val root = AbsolutePath(rootPath)
  logger.debug(s"Analyzing ${rootPath}")
  def apply(f: SemanticCtx => Result): Result = {
    import scala.collection.JavaConverters.asScalaIteratorConverter
    deleteOldFiles(root)
    val results = Files
      .walk(root.toNIO)
      .iterator()
      .asScala
      .filter { file =>
        Files.isRegularFile(file) &&
        PathIO.extension(file) == "semanticdb"
      }
      .toSeq
      .par
      .map { file =>
        SemanticDBFileVisitor(file, f)
      }
      .fold(Result.Empty)(mergeResults)
    results
  }
}

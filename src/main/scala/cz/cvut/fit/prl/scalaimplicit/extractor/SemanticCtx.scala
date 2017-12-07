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
    * Iterate through the parts of the fqn, starting from the root
    * package, in order to get the last symbol.
    * @param symbol The scalameta.Symbol whose representation we want
    * @return
    */
  def fetchReflectSymbol(symbol: Symbol): u.Symbol = {

    /**
      * Parse the scalameta fqns into reflection-compatible fqns, by
      *  - Removing the parameter list ("_root_.my.fun(I)I" => "_root_.my.fun")
      *  - Removing trailing dots and hashtags
      * @param symbol
      * @return
      */
    def wholeFqn(symbol: Symbol): String =
      symbol.syntax.split("""\(""").head.stripSuffix("#").stripSuffix(".")

    /**
      * We iterate through members: https://goo.gl/FvVABR,
      * returning all possible symbols that fit the fqn.
      * Note that this method is not suitable if the symbol has not been
      * loaded at least once before:
      * https://gist.github.com/blorente/77dd0a279ffd6b27d8d234e4424b6cda#getting-a-symbol-from-an-fqn
      * @param parent Symbol whose members we want to analyze
      * @param name Name of the member we are looking for
      * @return
      */
    def searchMember(parent: u.Symbol, memberNames: Seq[u.Name]): u.Symbol = {
      assert(parent != u.NoSymbol,
             s"Parent is empty with $memberNames left to search")
      if (memberNames.isEmpty) parent
      else {
        logger.debug(s"Searching ${memberNames.head} in $parent")
        // Recursively search for the new name
        searchMember(
          parent match {
            case p if p.isModule || p.isPackage =>
              p.asModule.moduleClass.info.member(memberNames.head)
            case p => p.typeSignature.member(memberNames.head)
          },
          memberNames.tail
        )
      }
    }

    /**
      * Extract a scala-reflect compatible fqn from a symbol.
      * Similar to wholeFqn, but returns a ReflectLoadable
      * @param symbol
      * @return
      */
    case class ReflectLoadable(owner: String, term: u.TermName)
    def splitFqn(symbol: Symbol): ReflectLoadable = {
      ReflectLoadable(
        symbol
          .productElement(0)
          .toString
          .stripPrefix("_empty_.")
          .stripPrefix("_root_.")
          .stripSuffix(".")
          .stripSuffix("#"),
        u.TermName(
          symbol
            .productElement(1)
            .toString
            .split("""\(""")
            .head
            .stripSuffix("#") // Strip trailing #s for scala meta type names in type parameters
        )
      )
    }

    /**
      * Given an owner and a term name to search, search for it with the mirror.
      * scala.reflect does not have a way to tell whether an fqn is a package, module or class.
      * Therefore, we have to try to load `pack` as each of those. https://goo.gl/MzBoJF
      *
      * It also may be that `name` is not there when we load something as a class, but
      * it appears when whe load it as a module (or vice versa).
      * Therefore, we have to keep looking if we don't find it at first.
      * FIXME: Exception-base flow control is obviously bad, we should look for a better way.
      * @param loadable Instance containing the owner and the term name to search
      * @return
      */
    def exceptionSearch(loadable: ReflectLoadable): u.Symbol = {
      def lookInClass: u.Symbol = {
        _mirror.staticClass(loadable.owner).typeSignature.member(loadable.term)
      }
      def lookInModule: u.Symbol = {
        _mirror
          .staticModule(loadable.owner)
          .moduleClass
          .info
          .member(loadable.term)
      }
      def lookInPackage: u.Symbol = {
        _mirror
          .staticPackage(loadable.owner)
          .moduleClass
          .info
          .member(loadable.term)
      }

      var ret: u.Symbol = u.NoSymbol
      ret = try { lookInClass } catch {
        case _: ScalaReflectionException =>
          logger.debug(s"${loadable.owner} is not a class")
          u.NoSymbol
      }
      if (ret == u.NoSymbol) {
        ret = try { lookInModule } catch {
          case _: ScalaReflectionException =>
            logger.debug(s"${loadable.owner} is not a module")
            u.NoSymbol
        }
        if (ret == u.NoSymbol) {
          ret = try { lookInPackage } catch {
            case _: ScalaReflectionException =>
              logger.debug(s"${loadable.owner} is not a package")
              u.NoSymbol
          }
        }
      }
      assert(
        ret != u.NoSymbol,
        s"We were able to load the package ${loadable.owner}, but unable to find ${loadable.term.toString}")
      ret
    }

    exceptionSearch(splitFqn(symbol))
    /*FIXME: If we find a way to bypass the guesswork on reflection (https://goo.gl/MzBoJF),
            we may want to use searchMember instead of exceptionSearch
    val names = wholeFqn(symbol).split("""[\.#]""").map(x => u.TermName(x))
    val res = names.head match {
      case u.TermName("_root_") =>
        searchMember(_mirror.RootPackage, names.tail)
      case u.TermName("_empty_") =>
        searchMember(_mirror.EmptyPackage, names.tail)
    }
   */
  }

  def signature(metaSymbol: Symbol): String = {
    val reflectSymbol = fetchReflectSymbol(metaSymbol)
    logger.debug(
      s"Reflective symbol lookup result: ${metaSymbol.syntax} -> ${reflectSymbol.toString}")
    reflectSymbol.toString
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

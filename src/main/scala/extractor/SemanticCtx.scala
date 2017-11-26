package extractor

import java.nio.file.{Files, Path}

import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb.{schema => s}

import scala.meta._
import scala.util.control.NonFatal


case class SemanticCtx(database: Database) {
  def input = database.documents.head.input
  val file: String = input match {
    case Input.VirtualFile(path, _) => path
    case Input.File(path, _) => path.toString
    case _ => ""
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
    case Importee.Rename(name, _) => symbol(name)
    case Importee.Name(name) => symbol(name)
    case Term.Select(_, name @ Name(_)) => symbol(name)
    case Type.Select(_, name @ Name(_)) => symbol(name)
    case _ => symbol(tree.pos)
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
        Console.withOut(Console.err) { println(s"[error] Function type unknown: ${other.structure}") }
        throw new RuntimeException()
      }
    }
  }

  def getKind(denot: Denotation): String = {
      var kind: String = denot match {
        case x if x.isVal => "val"
        case x if x.isVar => "var"
        case x if x.isDef => "def"
        case x if x.isTrait => "trait"
        case x if x.isMacro => "macro"
        case x if x.isObject => "object"
        case x if x.isVal && x.isLazy => "lazy val"
        case x if x.isClass && !x.isCase => "class"
        case x if x.isClass && x.isCase => "case class"
        case x if x.isParam => "param"
        case x if x.isPackage => "package"
        case x if x.isPackageObject => "package object"
        case x => s"<unknown: ${x.structure}>"
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
          case x if x.isClass && x.isCase => "case class"
          case x if x.isClass && !x.isCase => "class"
          case x if x.isObject => "object"
          case x if x.isTrait => "trait"
          case _ => ""
        }
        if (den.isImplicit) kind = s"implicit $kind"
        if (den.isFinal) kind = s"final $kind"
        if (den.isLazy) kind = s"lazy $kind"
        kind
      }
    }
  }
}

object SemanticDBFileVisitor extends ((Path, (SemanticCtx => Result)) => Result) {
  def apply(filePath: Path, f: SemanticCtx => Result): Result = {
      try {
        val sdb = s.Database.parseFrom(Files.readAllBytes(filePath))
        val mdb = sdb.toDb(None)
        val ctx = SemanticCtx(mdb)
        val res = f(ctx)
        print(".")
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
    files.forEach{file =>
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
        .map{case (_, v) => v.head}
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

class SingleProjectWalker(rootPath: String) extends TreeWalker {
  val root = AbsolutePath(rootPath)
  println(s"Analyzing ${rootPath}")
  def apply(f: SemanticCtx => Result): Result = {
    import scala.collection.JavaConverters._
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
        .map {file => SemanticDBFileVisitor(file, f)}
        .fold(Result.Empty) {(acc, partial) =>
          mergeResults(acc, partial)
        }
    println(" ")
    results
  }
}
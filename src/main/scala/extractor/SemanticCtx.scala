package extractor

import java.nio.file.{Files, Path}
import java.util.concurrent.CopyOnWriteArrayList

import org.langmeta.internal.io.PathIO

import scala.collection.mutable
import scala.meta._
import scala.util.control.NonFatal
import org.langmeta.internal.semanticdb.{schema => s}


case class SemanticCtx(database: Database, projectPath: AbsolutePath) {
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
    denot match {
      case x: Denotation if x.isVal && x.isLazy => "lazy val"
      case x: Denotation if x.isVal => "val"
      case x: Denotation if x.isVar => "var"
      case x: Denotation if x.isDef => "def"
      case x: Denotation if x.isObject => "object"
      case x: Denotation if x.isParam => "param"
      case x: Denotation if x.isMacro => "macro"
      case x: Denotation => s"<unknown: ${x.structure}>"
    }
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

object SemanticDBFileVisitor {
  def apply[T](filePath: Path, projectPath: AbsolutePath, f: SemanticCtx => T) = {
      try {
        val sdb = s.Database.parseFrom(Files.readAllBytes(filePath))
        val mdb = sdb.toDb(None)
        val ctx = SemanticCtx(mdb, projectPath)
        f(ctx)
        print(".")
      } catch {
        case NonFatal(e) =>
          val st = e.getStackTrace
          e.setStackTrace(st.take(10))
          e.printStackTrace()
      }
  }
}

trait SemanticDBWalker {
  def run[T](f: SemanticCtx => T): Unit
}

class SingleProjectWalker(rootPath: String) extends SemanticDBWalker {
  val root = AbsolutePath(rootPath)
  println(s"Analyzing: ${rootPath}")
  def run[T](f: SemanticCtx => T): Unit = {
    import scala.collection.JavaConverters._
    val files = Files
        .walk(root.toNIO)
        .iterator()
        .asScala
        .filter { file =>
          Files.isRegularFile(file) &&
            PathIO.extension(file) == "semanticdb"
        }
        .toVector
        .par
      files.foreach(x => SemanticDBFileVisitor(x, root, f))
  }
}

class MultipleProjectWalker(rootPath: String) extends SemanticDBWalker {
  val root = AbsolutePath(rootPath)

  def run[T](f: SemanticCtx => T): Unit = {    
    import scala.collection.JavaConverters._
    val dirs = Files.list(root.toNIO)
    dirs.forEach { project =>
      val projectPath = AbsolutePath(s"$rootPath/${project.getFileName}")
      println(s"Analyzing: ${project}")
      val files = Files
        .walk(project.toAbsolutePath)
        .iterator()
        .asScala
        .filter { file =>
          Files.isRegularFile(file) &&
            PathIO.extension(file) == "semanticdb"
        }
        .toVector
        .par
      files.foreach(x => SemanticDBFileVisitor(x, projectPath, f))
    }
  }
}

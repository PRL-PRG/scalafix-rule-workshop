package cz.cvut.fit.prl.scalaimplicit.extractor.contexts

import com.typesafe.scalalogging.LazyLogging

import scala.meta._

case class SemanticCtx(database: Database) extends LazyLogging {
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

  /**
    * We capture every instance of function applications
    * that may have implicit parameters.
    *
    * FIXME This list may not be exhaustive, but an assertion will be triggeded if a case is missed
    * There are some notable omissions, such as For and ForYield. The reason is that we
    * will not have symbols for them, and thus it is necessary to treat it as a special case
    *
    * ''NOTE'' That toMap will override entries with the same position
    */
  private lazy val _inSourceCallSites: Map[Int, Tree] =
    (tree collect {
      case x @ (_: Term.Apply | _: Term.ApplyInfix | _: Term.Select |
          _: Term.ApplyType | _: Term.ApplyUnary | _: Term.Interpolate) =>
        x.pos.end -> x
    }).toMap
  def inSourceCallSite(at: Int): Option[Tree] = _inSourceCallSites.get(at)

  /**
    * Filter for the synthetic function applications.
    * Captures all "apply()" functions inserted by the compiler.
    *
    * @param elem Synthetic to test
    * @return true iff the synthetic has apply in the name
    */
  private def hasApplyInTheName(elem: Synthetic): Boolean = {
    elem.text.startsWith("*.apply")
  }
  private lazy val _syntheticApplies: Map[Int, Synthetic] = {
    val applications =
      index.synthetics.filter(hasApplyInTheName).groupBy(_.position.end)
    assert(
      applications.forall(_._2.size == 1),
      s"There were multiple applies in position ${applications.find(_._2.size > 1).get._1}")
    applications
      .mapValues(_.head)
  }
  def syntheticApplication(at: Int): Option[Synthetic] =
    _syntheticApplies.get(at)

  /**
    * Synthetics with implicits in them.
    * They can be either conversions (`path.to.name(*)(implicit.param.one)`)
    * or parameter lists (`*(implicit.param.one,implicit.param.two)`).
    * The other sort of synthetics, do not contain parentheses
    * (synthetic applications - `*.apply` or type parameters - `*[Seq[Student]]`)
    */
  val syntheticsWithImplicits: Seq[Synthetic] =
    index.synthetics.filter(_.text.contains("("))

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

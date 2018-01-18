package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import com.typesafe.scalalogging.LazyLogging

import scala.collection.{immutable, mutable}
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
    * Fix for:
    * https://github.com/PRL-PRG/scalafix-rule-workshop/issues/39
    *
    * Tuned on purpose to be as narrow as possible
    *
    * We search for the function that the recursive call refers to.
    *
    * Starting from the position of the term, we go backwards the list of names
    * until we find one that (1) isDefinition and (2) has a denotation.name that
    * coincides with the one on t.name
    * @param t
    * @return
    */
  def unrecurse(t: Term.Name): Option[Symbol] = {
    val sortedNames =
      names.filter(_.position.end <= t.pos.end).sortBy(_.position.end).reverse
    val symbol = sortedNames
      .find(x => {
        val d = denotation(x.symbol)
        x.isDefinition && d.isDefined && d.get.name == t.value
      })
      .getOrElse {
        throw new RuntimeException(s"Could not unrecurse ${t}")
      }
      .symbol
    Some(symbol)
  }

  /**
    * We capture every instance of function applications
    * that may have implicit parameters.
    *
    * FIXME This list may not be exhaustive, but an assertion will be triggeded if a case is missed
    * There are some notable omissions, such as For and ForYield. The reason is that we
    * will not have symbols for them, and thus it is necessary to treat it as a special case
    *
    * We also match from the outside in, so for each position we keep only the outermost term.
    * This allows us to gather Term.Names only when there are no actual applications in that position.
    * To gather this we trust the traversal order of collect to process the outer terms before the
    * inner terms, as demonstrated in this Ammonite session:
    * @ val tr = "my.fun.app(Hello)".parse[Stat].get
    * @ tr.collect {case a: Term.Select => println(s"Sel ${a}"); case a: Term.Apply => println(s"App $a"); case a: Term.Name => println(s"Nam $a") }
    * App my.fun.app(Hello)
    * Sel my.fun.app
    * Sel my.fun
    * Nam my
    * Nam fun
    * Nam app
    * Nam Hello
    *
    * Note that we can safely use toMap, since every position will be there only once.
    */
  private val _inSourceCallSites: Map[Int, Tree] = {
    var positions = mutable.MutableList[Int]()
    (tree collect {
      case x
          if SemanticCtx.isApplication(x) && !positions
            .contains(x.pos.end) => {
        positions += x.pos.end
        x.pos.end -> x
      }
      case x: Init
          if (x.argss.isEmpty || x.argss.last.isEmpty) && !positions.contains(
            x.pos.end - 1) => {
        // For Inits without parameters, we take the last position minus one,
        // which seems consistent with empiric observations
        val truePos = x.pos.end - 1
        positions += truePos
        truePos -> x
      }
      case x: Init
          if x.argss.nonEmpty && !positions.contains(
            x.argss.last.last.pos.end) => {
        // For Inits, the position where an implicit param list
        // will be inserted is the same as the end position
        // of the last parameter
        val truePos = x.argss.last.last.pos.end
        positions += truePos
        truePos -> x
      }
    }).toMap
  }
  def inSourceCallSite(at: Int): Option[Tree] = _inSourceCallSites.get(at)

  /*
  // This assumes that every symbol has a denotation.
  // If a symbol doesn't, the .get method fails
  val inSourceDefinitions: Seq[(Position, Symbol, Denotation)] = {
    _names
      .filter(_._2.isDefinition)
      .map(x => (x._1, x._2.symbol, denotation(x._2.symbol).get))
      .toSeq
  }
   */
  val inSourceDefinitions: Seq[Tree] = tree.collect {
    case d: Defn => d
    case d: Decl => d
  }

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
}

// Static functions, mostly that provide checks with semanticdb
// That do not require state
object SemanticCtx {
  def isApplication(t: Tree) = t match {
    case x @ (_: Term.Apply | _: Term.ApplyInfix | _: Term.Select |
        _: Term.ApplyType | _: Term.ApplyUnary | _: Term.Interpolate |
        _: Term.New | _: Term.NewAnonymous | _: Term.Name) =>
      true
    case x => false
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

  // Detect local type references [...] A => some.Code()L/Ret;.A#
  def isLocalTypeReference(s: Symbol.Global): Boolean =
    s.signature.isInstanceOf[Signature.Type] && s.owner.syntax.endsWith(";.")
}

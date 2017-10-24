package implicits

import java.nio.file.{ Files, Paths, StandardOpenOption }

import scalafix.syntax._
import scala.meta._
import scala.meta.contrib._
import scalafix.{ Patch, RuleCtx, SemanticRule, SemanticdbIndex }

object CSV {

  trait Serializable[A] {
    def csvHeader: Seq[String]
    def csvValues: Seq[String]
    def id: String
  }

  def writeCSV[A](xs: Iterable[_ <: Serializable[A]], path: String): Unit = {
    def prepareValue(x: String) = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n").replaceAll("\"", "'") + '"'
    }

    if (xs.nonEmpty) {
      val header = xs.head.csvHeader.mkString(",")
      val values =
        xs.map(_.csvValues.map(prepareValue).mkString(","))
          .mkString("\n") +
          "\n"

      if (Files.exists(Paths.get(path))) {
        Files.write(Paths.get(path), s"$values".getBytes, StandardOpenOption.APPEND)
      } else {
        Files.write(Paths.get(path), s"$header\n$values".getBytes)
      }
    }
  }
}

final case class ImplicitContextCSV(index: SemanticdbIndex)
  extends SemanticRule(index, "ImplicitContext") {

  final case class ImplicitParam(symbol: Symbol, denot: Denotation) extends CSV.Serializable[ImplicitParam] {
    lazy val id: String = s"${symbol.syntax}"
    // If there are no names on the denotation, we have an implicit which defines
    // its own type - Most likely an object
    val name: String = denot.name.toString
    val clazz: String = denot.names.headOption match {
                          case Some(n) => n.symbol.toString
                          case None => symbol.toString
                        }
    val typee: String = denot.names.headOption match {
                          case Some(n) => denot.signature.toString
                          case None => name
                        }
    val kind: String = getKind(denot)

    val typeKind: String = denot.names.headOption match {
                            case Some(n) => getTypeKind(n.symbol)
                            case None => getTypeKind(symbol)
                          }

    override val csvHeader: Seq[String] = Seq("id", "clazz", "type", "kind", "name", "type-description")
    override val csvValues: Seq[String] = Seq(id, clazz, typee, kind, name, typeKind)

    def getKind(denot: Denotation): String = {
      denot match {
        case x: Denotation if x.isVal && x.isLazy => "lazy val"
        case x: Denotation if x.isVal => "val"
        case x: Denotation if x.isVar => "var"
        case x: Denotation if x.isDef => "def"
        case x: Denotation if x.isObject => "object"
        case x: Denotation if x.isParam => "param"
        case x: Denotation => s"<unknown: ${x.structure}>"
      }
    }

    def getTypeKind(symbol: Symbol): String = {
      symbol.denotation match {
        case Some(den) => {
          var kind = den match {
            case x if x.isClass && x.isCase => "case class"
            case x if x.isClass && !x.isCase => "class"
            case x if x.isObject => "object"
            case x if x.isTrait => "trait"
          }
          if (den.isImplicit) kind = s"implicit $kind"
          if (den.isFinal) kind = s"final $kind"
          if (den.isLazy) kind = s"lazy $kind"
          kind
        }
        case None => "<no denotation>"
      }
    }

  }

  final case class AppTerm(term: Term, params: Int)
  final case class FunApply(app: AppTerm, file: String) extends CSV.Serializable[FunApply] {
    lazy val id: String = s"$file:$line:$col"

    // Take end line and cols because function call chains have the same start
    val line: String = app.term.pos.endLine.toString
    val col: String = app.term.pos.endColumn.toString
    val symbol: String = qualifiedName(app.term)
    val code: String = app.term.toString
    val nargs = app.params.toString

    override val csvHeader: Seq[String] = Seq("id", "path", "line", "col", "code", "symbol", "nargs")
    override val csvValues: Seq[String] = Seq(id, file, line, col, code, symbol, nargs)
  }

  final case class SyntheticApply(synth: Synthetic, file: String, params: Int) extends CSV.Serializable[FunApply] {
    lazy val id: String = s"$file:$line:$col"

    // Take end line and cols because function call chains have the same start
    val line: String = synth.position.endLine.toString
    val col: String = synth.position.endColumn.toString
    val symbol: String = synth.names(1).symbol.toString
    val code: String = s"apply(${if (params > 0) { "_" + ",_" * (params - 1) }})"

    override val csvHeader: Seq[String] = Seq("id", "path", "line", "col", "code", "symbol", "nargs")
    override val csvValues: Seq[String] = Seq(id, file, line, col, code, symbol, params.toString)
  }

  def qualifiedName(symbol: Term): String = {
    symbol match {
      case fun: Term.Name => {
        s"${fun.symbol.getOrElse(s"<unknown fun: ${fun}>")}"
      }
      case fun: Term.Select => {
        s"${fun.name.symbol.getOrElse(qualifiedName(fun.name))}"
      }
      case fun: Term.ApplyType => {
        qualifiedName(fun.fun)
      }
      case fun: Term.Apply => {
        fun.symbol.getOrElse(qualifiedName(fun.fun)).toString
      }
      case other => {
        Console.withOut(Console.err) { println(s"[error] Function type unknown: ${other.structure}") }
        throw new RuntimeException()
      }
    }
  }

  final case class FunApplyWithImplicitParam[A, B](fun: CSV.Serializable[A], param: CSV.Serializable[B])
    extends CSV.Serializable[FunApplyWithImplicitParam[A, B]] {

    val from: String = param.id
    val to: String = fun.id

    override val csvHeader: Seq[String] = Seq("from", "to")
    override val csvValues: Seq[String] = Seq(from, to)

    override def id: String = "None"
  }

  override def fix(ctx: RuleCtx): Patch = {
    val file: String = ctx.input match {
      case Input.VirtualFile(path, _) => path
      case Input.File(path, _) => path.toString
      case _ => ""
    }
    val syntheticImplicits =
      for {
        syn <- ctx.index.synthetics
        name <- syn.names
        symbol = name.symbol
        den <- symbol.denotation if den.isImplicit
      } yield {
        syn -> ImplicitParam(symbol, den)
      }

    val paramsFuns =
      for {
        app <- ctx.tree collect {
          case x: Term.Apply => AppTerm(x, x.args.size)
          case x: Term.Name => AppTerm(x, 0)
        }
        param <- syntheticImplicits collect {
          case (syn, den) if syn.position.end == app.term.pos.end => den
        }
        syntheticApply = ctx.index.synthetics find {
          x => x.text.toString.equals("*.apply") && x.position.end >= app.term.pos.start && x.position.end <= app.term.pos.end
        }
      } yield {
        syntheticApply match {
          case Some(synth) => FunApplyWithImplicitParam(SyntheticApply(synth, file, app.params), param)
          case None => FunApplyWithImplicitParam(FunApply(app, file), param)
        }
      }

    val params = paramsFuns.groupBy(_.param).keys.toSet
    val funs = paramsFuns.groupBy(_.fun).keys

    CSV.writeCSV(params, "params.csv")
    CSV.writeCSV(funs, "funs.csv")
    CSV.writeCSV(paramsFuns, "params-funs.csv")

    Patch.empty
  }
}

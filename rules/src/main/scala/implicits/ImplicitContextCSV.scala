package implicits

import java.nio.file.{Files, Paths, StandardOpenOption}

import scalafix.syntax._
import scala.meta._
import scala.meta.contrib._
import scalafix.{Patch, RuleCtx, SemanticRule, SemanticdbIndex}

object CSV {

  trait Serializable[A] {
    def csvHeader: Seq[String]
    def csvValues: Seq[String]
  }

  def writeCSV[A](xs: Iterable[_ <: Serializable[A]], path: String): Unit = {
    def prepareValue(x: String) = {
      // FIXME: properly escape " in x
      '"' + x.replaceAll("\n", "\\\\n") + '"'
    }

    if (xs.nonEmpty) {
      val header = xs.head.csvHeader.mkString(", ")
      val values =
        xs.map(_.csvValues.map(prepareValue).mkString(", "))
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
    val id: String = symbol.syntax
    val clazz: String = denot.names.head.symbol.toString
    val tipe: String = denot.signature

    override val csvHeader: Seq[String] = Seq("id", "clazz", "type")
    override val csvValues: Seq[String] = Seq(id, clazz, tipe)
  }

  final case class FunApply(app: Term.Apply) extends CSV.Serializable[FunApply] {
    lazy val id: String = s"$file:$line:$col"
    val file: String = app.pos.input match {
      case Input.VirtualFile(path, _) => path
      case Input.File(path, _) => path.toString
      case _ => ""
    }
    // Take end line and cols because function call chains have the same start
    val line: String = app.pos.endLine.toString
    val col: String = app.pos.endColumn.toString
    // TODO: resolve fqn
    val symbol: String = app.fun.symbol.map(_.toString).getOrElse("<unknown>")
    val code: String = app.toString

    override val csvHeader: Seq[String] = Seq("id", "symbol", "code")
    override val csvValues: Seq[String] = Seq(id, symbol, code)
  }

  final case class FunApplyWithImplicitParam(fun: FunApply, param: ImplicitParam)
    extends CSV.Serializable[FunApplyWithImplicitParam] {

    val from: String = param.id
    val to: String = fun.id

    override val csvHeader: Seq[String] = Seq("from", "to")
    override val csvValues: Seq[String] = Seq(from, to)
  }

  override def fix(ctx: RuleCtx): Patch = {
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
          case x: Term.Apply => x
        }
        param <- syntheticImplicits collect {
          case (syn, den) if syn.position.end == app.pos.end => den
        }
      } yield {
        FunApplyWithImplicitParam(FunApply(app), param)
      }

    val params = paramsFuns.groupBy(_.param).keys
    val funs = paramsFuns.groupBy(_.fun).keys

    CSV.writeCSV(params, "params.csv")
    CSV.writeCSV(funs, "funs.csv")
    CSV.writeCSV(paramsFuns, "params-funs.csv")

    Patch.empty
  }
}
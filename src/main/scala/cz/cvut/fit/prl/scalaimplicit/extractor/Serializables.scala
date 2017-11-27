package cz.cvut.fit.prl.scalaimplicit.extractor

import scala.meta._

trait ResultElement {
  def id: String
}

final case class Location(path: String, line: String, col: String) {
  val sourcelink = s"$path:$line:$col"
}
object Location {
  val Empty = Location("dummy/path.semanticdb", "-1", "-1")
}

object Serializables {

  final case class ImplicitParam(fqn: String, fqtn: String, signature: String, kind: String, plainName: String) extends ResultElement with CSV.Serializable {
    lazy val id: String = fqn
    override val csvHeader: Seq[String] = Seq("fqn", "fqfn", "fqparamlist", "fqtn", "type", "kind", "name")
    override val csvValues: Seq[String] = Seq(fqn, "", "", fqtn, signature, kind, plainName)
  }

  def createImplicitParam(ctx: SemanticCtx, symbol: Symbol, denot: Denotation): ImplicitParam = {
    ImplicitParam(
      fqn = s"${symbol.syntax}",
      plainName = denot.name.toString,
      fqtn = denot.names.headOption match {
        case Some(n) => n.symbol.toString
        case None => symbol.toString
      },
      // If there are no names on the denotation, we have an implicit which defines
      // its own type - Most likely an object
      signature = denot.names.headOption match {
        case Some(n) => denot.signature.toString
        case None => denot.name.toString
      },
      kind = ctx.getKind(denot)
    )
  }

  final case class Apply(location: Location, code: String, fqn: String, nargs: String) extends ResultElement with CSV.Serializable {
    override lazy val id = location.sourcelink
    override val csvHeader: Seq[String] = Seq("sourcelink", "path", "line", "col", "code", "symbol", "fqfn", "fqparamlist", "nargs")
    override val csvValues: Seq[String] = Seq(id, location.path, location.line, location.col, code, fqn, "", "", nargs)
  }
  final case class AppTerm(term: Term, params: Int, nameEnd: Int)
  def createFunctionApplication(ctx: SemanticCtx, app: AppTerm, file: String): Apply = {
    Apply(
      location = Location(file, app.term.pos.endLine.toString, app.term.pos.endColumn.toString),
      code = app.term.toString,
      fqn = ctx.qualifiedName(app.term),
      nargs = app.params.toString
    )
  }
  def createSyntheticApplication(ctx: SemanticCtx, synth: Synthetic, file: String, params: Int): Apply = {
    Apply(
      location = Location(file, synth.position.endLine.toString, synth.position.endColumn.toString),
      code = s"apply(${
        if (params > 0) {
          "_" + ",_" * (params - 1)
        }
      })",
      fqn = synth.names(1).symbol.toString,
      nargs = params.toString
    )
  }

  final case class FunApplyWithImplicitParam(from: String, to: String) extends ResultElement with CSV.Serializable {
    override def id: String = s"($from, $to)"
    override val csvHeader: Seq[String] = Seq("from", "to")
    override val csvValues: Seq[String] = Seq(from, to)
  }
  def createLink(param: ImplicitParam, fun: Apply): FunApplyWithImplicitParam = {
    FunApplyWithImplicitParam(
      from = param.id,
      to = fun.id
    )
  }

  final case class DeclaredImplicit(location: Location, fqn: String, plainName: String, kind: String, fqtn: String, signature: String, nargs: String) extends ResultElement with CSV.Serializable {
    override lazy val id: String = fqn
    override val csvHeader: Seq[String] = Seq("sourcelink", "path", "line", "col", "name", "fqn", "fqtn", "type", "kind", "nargs")
    override val csvValues: Seq[String] = Seq(location.sourcelink, location.path, location.line, location.col, plainName, fqn, fqtn, signature, kind, nargs)
  }
  def createDeclaredImplicit(ctx: SemanticCtx, name: ResolvedName, denot: Denotation, path: String): DeclaredImplicit = {
    DeclaredImplicit(
      location = Location(path=path, line=name.position.endLine.toString, col=name.position.endColumn.toString),
      fqn = name.symbol.syntax,
      plainName = denot.name.toString,
      kind = ctx.getKind(denot),
      fqtn = denot.names.headOption match {
        case Some(n) => n.symbol.toString
        case None => denot.name.toString
      },
      signature = denot.names.headOption match {
        case Some(n) => denot.signature.toString
        case None => denot.name.toString
      },
      nargs =
        if (ctx.getKind(denot) == "def") denot.members.toString
        else "-1"
    )
  }

}

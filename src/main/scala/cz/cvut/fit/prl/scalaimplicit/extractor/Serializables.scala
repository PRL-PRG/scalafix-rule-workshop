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
  def resolve(denotation: Denotation, symbol: Symbol): String = {
    def substitute(current: (String, Int), rn: ResolvedName) = {
      val (s, offset) = current
      val substituted = s.substring(0, rn.position.start + offset) + // all before symbol
        rn.symbol + // resolved name
        s.substring(rn.position.end + offset, s.length) // all after symbol

      (substituted,
       offset + rn.symbol.toString.length - (rn.position.end - rn.position.start))
    }

    denotation.names
      .filter(_.symbol.productElement(0) != symbol) // discard the local type references (e.g. T => ...#.[T];)
      .foldLeft((denotation.signature, 0))(substitute)
      ._1
  }

  final case class ImplicitParam(fqn: String, signature: String, kind: String)
      extends ResultElement
      with CSV.Serializable {
    lazy val id: String = fqn
    override val csvHeader: Seq[String] =
      Seq("fqn", "name", "fqfn", "fqparamlist", "signature", "kind")
    override val csvValues: Seq[String] =
      Seq(fqn, "", "", "", signature, kind)
  }

  def createImplicitParam(ctx: SemanticCtx,
                          symbol: Symbol,
                          denot: Denotation): ImplicitParam = {
    ImplicitParam(
      fqn = s"${symbol.syntax}",
      signature = resolve(denot, symbol),
      kind = ctx.getKind(denot)
    )
  }

  final case class Apply(location: Location,
                         code: String,
                         fqn: String,
                         nargs: String)
      extends ResultElement
      with CSV.Serializable {
    override lazy val id = location.sourcelink
    override val csvHeader: Seq[String] = Seq("sourcelink",
                                              "path",
                                              "line",
                                              "col",
                                              "code",
                                              "symbol",
                                              "fqfn",
                                              "fqparamlist",
                                              "nargs")
    override val csvValues: Seq[String] = Seq(id,
                                              location.path,
                                              location.line,
                                              location.col,
                                              code,
                                              fqn,
                                              "",
                                              "",
                                              nargs)
  }
  final case class AppTerm(term: Term, params: Int, nameEnd: Int)
  def createFunctionApply(ctx: SemanticCtx,
                          app: AppTerm,
                          file: String): Apply = {
    Apply(
      location = Location(file,
                          app.term.pos.endLine.toString,
                          app.term.pos.endColumn.toString),
      code = app.term.toString,
      fqn = ctx.qualifiedName(app.term),
      nargs = app.params.toString
    )
  }
  def createSyntheticApply(ctx: SemanticCtx,
                           synth: Synthetic,
                           file: String,
                           params: Int): Apply = {
    Apply(
      location = Location(file,
                          synth.position.endLine.toString,
                          synth.position.endColumn.toString),
      code = s"apply(${Seq.fill(params)("_").mkString(", ")})",
      fqn = synth.names(1).symbol.toString,
      nargs = params.toString
    )
  }

  final case class FunApplyWithImplicitParam(from: String, to: String)
      extends ResultElement
      with CSV.Serializable {
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

  final case class DeclaredImplicit(location: Location,
                                    fqn: String,
                                    kind: String,
                                    signature: String,
                                    nargs: String)
      extends ResultElement
      with CSV.Serializable {
    override lazy val id: String = fqn
    override val csvHeader: Seq[String] =
      Seq("sourcelink",
          "path",
          "line",
          "col",
          "fqn",
          "name",
          "signature",
          "kind",
          "nargs")
    override val csvValues: Seq[String] = Seq(location.sourcelink,
                                              location.path,
                                              location.line,
                                              location.col,
                                              fqn,
                                              "",
                                              signature,
                                              kind,
                                              nargs)
  }
  def createDeclaredImplicit(ctx: SemanticCtx,
                             name: ResolvedName,
                             denot: Denotation,
                             path: String): DeclaredImplicit = {
    DeclaredImplicit(
      location = Location(path = path,
                          line = name.position.endLine.toString,
                          col = name.position.endColumn.toString),
      fqn = name.symbol.syntax,
      kind = ctx.getKind(denot),
      signature = resolve(denot, name.symbol),
      nargs =
        if (ctx.getKind(denot) == "def") denot.members.toString
        else "-1"
    )
  }

}

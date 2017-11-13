package extractor

import scala.meta._

final case class ImplicitParam(ctx: SemanticCtx, symbol: Symbol, denot: Denotation) extends CSV.Serializable {
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
  val kind: String = ctx.getKind(denot)

  override val csvHeader: Seq[String] = Seq("fqn", "class", "type", "kind", "name")
  override val csvValues: Seq[String] = Seq(id, clazz, typee, kind, name)

}

final case class AppTerm(term: Term, params: Int, nameEnd: Int)
final case class FunApply(ctx: SemanticCtx, app: AppTerm, file: String) extends CSV.Serializable {
  lazy val id: String = s"$file:$line:$col"

  // Take end line and cols because function call chains have the same start
  val line: String = app.term.pos.endLine.toString
  val col: String = app.term.pos.endColumn.toString
  val symbol: String = ctx.qualifiedName(app.term)
  val code: String = app.term.toString
  val nargs = app.params.toString

  override val csvHeader: Seq[String] = Seq("id", "path", "line", "col", "code", "symbol", "fqfn", "fqparamlist", "nargs")
  override val csvValues: Seq[String] = Seq(id, file, line, col, code, symbol, "", "", nargs)
}

final case class SyntheticApply(ctx: SemanticCtx, synth: Synthetic, file: String, params: Int) extends CSV.Serializable {
  lazy val id: String = s"$file:$line:$col"

  // Take end line and cols because function call chains have the same start
  val line: String = synth.position.endLine.toString
  val col: String = synth.position.endColumn.toString
  val symbol: String = synth.names(1).symbol.toString
  val code: String = s"apply(${if (params > 0) { "_" + ",_" * (params - 1) }})"

  override val csvHeader: Seq[String] = Seq("id", "path", "line", "col", "code", "symbol", "fqfn", "symbol", "nargs")
  override val csvValues: Seq[String] = Seq(id, file, line, col, code, symbol, "", "", params.toString)
}


final case class FunApplyWithImplicitParam(fun: CSV.Serializable, param: CSV.Serializable) extends CSV.Serializable {

  val from: String = param.id
  val to: String = fun.id

  override val csvHeader: Seq[String] = Seq("from", "to")
  override val csvValues: Seq[String] = Seq(from, to)

  override def id: String = "None"
}

final case class DeclaredImplicit(ctx: SemanticCtx, name: ResolvedName, denot: Denotation, file: String) extends CSV.Serializable {

  val path: String = file
  val line: String = name.position.endLine.toString
  val col: String = name.position.endColumn.toString
  lazy val id: String = s"$path:$line:$col"

  val fqn: String = name.symbol.syntax
  val plainName: String = denot.name.toString
  val kind: String = ctx.getKind(denot)
  val clazz: String = denot.names.headOption match {
    case Some(n) => n.symbol.toString
    case None => denot.name.toString
  }
  val typee: String = denot.names.headOption match {
    case Some(n) => denot.signature.toString
    case None => name.symbol.toString
  }

  override val csvHeader: Seq[String] = Seq("id", "path", "line", "col", "name", "fqn", "class", "type", "kind")
  override val csvValues: Seq[String] = Seq(id, path, line, col, plainName, fqn, clazz, typee, kind)
}

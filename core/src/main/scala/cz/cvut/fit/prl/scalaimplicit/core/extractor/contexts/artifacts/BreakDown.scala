package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import scala.meta.{Position, Symbol}

case class TArg(symbol: Symbol, args: Seq[TArg])
case class QualifiedSymbol(app: Option[Symbol], isSynthetic: Boolean)
object QualifiedSymbol {
  val Empty = QualifiedSymbol(None, false)
}

case class BreakDown(symbol: QualifiedSymbol,
                     typeParams: Seq[TArg],
                     params: Seq[Param],
                     pos: Position,
                     code: String = "")
    extends Param

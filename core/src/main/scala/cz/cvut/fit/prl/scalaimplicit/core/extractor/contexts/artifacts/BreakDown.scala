package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import scala.meta.{Position, Symbol, Type, Synthetic}

case class QualifiedSymbol(app: Option[Symbol], isSynthetic: Boolean)
object QualifiedSymbol {
  val Empty = QualifiedSymbol(None, false)
}

case class BreakDown(symbol: QualifiedSymbol,
                     targs: Seq[Type],
                     args: Seq[Param],
                     pos: Position,
                     code: String = "")
    extends Param
case class SyntheticBreakdown(
    breakDown: BreakDown,
    applicationSynthetic: Option[Synthetic] = None,
    paramListSynthetic: Option[Synthetic] = None
)

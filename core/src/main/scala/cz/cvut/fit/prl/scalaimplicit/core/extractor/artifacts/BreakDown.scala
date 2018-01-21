package cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts

import scala.meta.{Position, Symbol, Type, Synthetic, Denotation}

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

case class SyntheticOrigins(application: Option[Synthetic] = None,
                            paramList: Option[Synthetic] = None)
object SyntheticOrigins {
  val Empty = SyntheticOrigins()
}
case class SyntheticBreakdown(
    breakDown: BreakDown,
    origins: SyntheticOrigins = SyntheticOrigins.Empty
)

case class DefnBreakdown(
    pos: Position,
    sym: scala.reflect.runtime.universe.Symbol,
    den: Option[Denotation]
)

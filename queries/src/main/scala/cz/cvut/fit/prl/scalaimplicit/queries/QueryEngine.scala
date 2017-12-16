package cz.cvut.fit.prl.scalaimplicit.queries

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite
import cz.cvut.fit.prl.scalaimplicit.macros.QueryEngineMacros

object QueriableRepresentation {
  private def rep(of: Option[String]): String = of.getOrElse("_")
  case class QCS(name: Option[String] = None,
                 code: Option[String] = None,
                 location: Option[String] = None,
                 isSynthetic: Option[String] = None,
                 declaration: Option[String] = None,
                 typeArguments: Option[String] = None,
                 implicitArguments: Option[String] = None) {
    lazy val repString: String = {
      s"CallSite(${rep(name)},${rep(code)},${rep(location)},${rep(isSynthetic)},${rep(
        declaration)},${rep(typeArguments)},${rep(implicitArguments)})"
    }
  }
}

object QueryEngine {
  import QueriableRepresentation._
  def apply(q: QCS, res: ExtractionResult): Seq[CallSite] =
    res.callSites.filter(cs => QueryEngineMacros.coincides(cs, q.repString))
}

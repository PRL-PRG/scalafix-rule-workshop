package cz.cvut.fit.prl.scalaimplicit.core.extractor.decomposers

import scala.meta.{Pat, Tree, Symbol}

object PatDecomposer {

  def handleUseless(p: Pat)(implicit finder: Tree => Symbol): Seq[Symbol] =
    Seq()

  def handleMultiplePats(args: Pat*)(
      implicit finder: Tree => Symbol): Seq[Symbol] =
    args.flatMap(PatDecomposer(_))

  def apply(pat: Pat)(implicit finder: Tree => Symbol): Seq[Symbol] = {
    pat match {
      case p: Pat.Var => Seq(finder(p.name))
      case p: Pat.Wildcard => handleUseless(p)
      case p: Pat.SeqWildcard => handleUseless(p)
      case p: Pat.Bind => handleMultiplePats(p.lhs)
      case p: Pat.Alternative => handleMultiplePats(p.lhs, p.rhs)
      case p: Pat.Tuple => handleMultiplePats(p.args: _*)
      case p: Pat.Extract => handleMultiplePats(p.args: _*)
      case p: Pat.ExtractInfix => handleMultiplePats(p.lhs :: p.rhs: _*)
      case p: Pat.Interpolate =>
        throw new MatchError(s"Example of Pat.Interpolate: ${p}")
      case p: Pat.Xml => throw new MatchError(s"Example of Pat.Xml: ${p}")
      case p: Pat.Typed => handleMultiplePats(p.lhs)
    }
  }
}

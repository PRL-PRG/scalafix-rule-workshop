package cz.cvut.fit.prl.scalaimplicit.core.extractor.decomposers

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts.DefnBreakdown

import scala.meta.{Decl, Defn, Pat, Term, Tree, Type, Symbol}

object DefnDecomposer {

  def handlePats(d: { def pats: Seq[Pat] })(
      implicit finder: Tree => Symbol): Seq[Symbol] = {
    assert(d.pats.size == 1,
           s"pattern Defn/Decl found with more than one pattern: $d")
    PatDecomposer(d.pats.head)
  }

  def handleTermName(d: { def name: Term.Name })(
      implicit finder: Tree => Symbol): Seq[Symbol] =
    Seq(finder(d.name))

  def handleTypeName(d: { def name: Type.Name })(
      implicit finder: Tree => Symbol): Seq[Symbol] =
    Seq(finder(d.name))

  def apply(ctx: ReflectiveCtx, defn: Tree)(
      implicit finder: Tree => Symbol): Seq[DefnBreakdown] = {
    val metaSymbols: Seq[Symbol] = defn match {
      // Definitions
      case d: Defn.Val => handlePats(d)
      case d: Defn.Var => handlePats(d)
      case d: Defn.Def => handleTermName(d)
      case d: Defn.Macro => handleTermName(d)
      case d: Defn.Object => handleTermName(d)
      case d: Defn.Class => handleTypeName(d)
      case d: Defn.Type => handleTypeName(d)
      case d: Defn.Trait => handleTypeName(d)
      // Declarations
      case d: Decl.Val => handlePats(d)
      case d: Decl.Var => handlePats(d)
      case d: Decl.Def => handleTermName(d)
      case d: Decl.Type => handleTypeName(d)
    }
    metaSymbols.map(
      metaSymbol =>
        DefnBreakdown(
          pos = defn.pos,
          den = ctx.denotation(metaSymbol),
          sym = ctx.findReflectSymbol(metaSymbol)
      ))
  }
}

package cz.cvut.fit.prl.scalaimplicit.core.extractor.decomposers

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts.DefnBreakdown

import scala.meta.{Decl, Defn, Pat, Symbol, Term, Tree, Type}
import scala.util.{Failure, Success, Try}

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
    val metaSymbols: Seq[Symbol] = (defn match {
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
    }) //.filterNot(x => SemanticCtx.isKnowCornerCase(defn, x))
    metaSymbols.map {
      case metaSymbol @ (s: Symbol.Global) =>
        Try(ctx.reflectOnDefn(s)) match {
          case Success(sym) =>
            DefnBreakdown(defn.pos, Some(sym), ctx.denotation(metaSymbol))
        }
      case metaSymbol =>
        DefnBreakdown(defn.pos, None, ctx.denotation(metaSymbol))
    }

    /*
      Try(ctx.reflectOnDefn(metaSymbol.asInstanceOf[Symbol.Global])) match {
        case Success(s) =>
          DefnBreakdown(
            pos = defn.pos,
            den = ctx.denotation(metaSymbol),
            sym = Some(s)
          )
        case Failure(x) if x.isInstanceOf[ClassCastException] =>
          DefnBreakdown(
            pos = defn.pos,
            den = ctx.denotation(metaSymbol),
            sym = None
          )
      }
      })*/
  }
}

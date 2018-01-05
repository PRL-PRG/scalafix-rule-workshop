package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import org.langmeta.inputs.Position
import org.langmeta.semanticdb.Denotation

import scala.reflect.runtime.{universe => u}

case class ReflectiveTArg(reflection: u.Symbol, args: Seq[ReflectiveTArg])
object ReflectiveTArg {
  def apply(ctx: ReflectiveCtx, targ: TArg): ReflectiveTArg =
    new ReflectiveTArg(
      reflection = ctx.findReflectSymbol(targ.symbol),
      args = targ.args.map(ReflectiveTArg(ctx, _))
    )
}
case class Reflection(originalSymbol: QualifiedSymbol,
                      isImplicit: Boolean,
                      fullName: String,
                      kind: String,
                      baseClasses: List[Reflection],
                      params: Seq[Param],
                      typeArguments: Seq[ReflectiveTArg],
                      declarationPos: Position,
                      pos: Position,
                      code: String,
                      typeSignature: u.Type,
                      paramLists: List[List[u.Symbol]],
                      returnType: u.Type)
    extends Param
object Reflection {
  def apply(ctx: ReflectiveCtx, bd: BreakDown, ref: u.Symbol): Reflection =
    new Reflection(
      originalSymbol = bd.symbol,
      isImplicit = ref.isImplicit,
      fullName = ref.fullName,
      kind = ctx.getReflectiveKind(ref),
      baseClasses = baseClasses(ctx, bd, ref),
      pos = bd.pos,
      declarationPos = Position.None,
      code = bd.code,
      params = bd.params.map(reflectiveParam(ctx, _)),
      typeArguments = bd.typeParams.map(ReflectiveTArg(ctx, _)),
      typeSignature = ref.typeSignature,
      paramLists = ctx.paramLists(ref),
      returnType = ctx.returnType(ref)
    )

  def apply(ctx: ReflectiveCtx,
            bd: BreakDown,
            den: Denotation,
            ref: u.Symbol): Reflection =
    new Reflection(
      originalSymbol = bd.symbol,
      isImplicit = den.isImplicit,
      fullName = ref.fullName,
      kind = ctx.getKind(den),
      baseClasses = baseClasses(ctx, bd, ref),
      pos = bd.pos,
      declarationPos = Position.None,
      code = bd.code,
      params = bd.params.map(reflectiveParam(ctx, _)),
      typeArguments = bd.typeParams.map(ReflectiveTArg(ctx, _)),
      typeSignature = ref.typeSignature,
      paramLists = ctx.paramLists(ref),
      returnType = ctx.returnType(ref)
    )


  def reflectiveParam(ctx: ReflectiveCtx, param: Param): Param = {
    param match {
      case bd: BreakDown => ctx.findReflection(bd)
      case p: Param => p
    }
  }

  def baseClasses(ctx: ReflectiveCtx,
                  bd: BreakDown,
                  ref: u.Symbol): List[Reflection] = {
    ctx.firstLevelBaseClasses(ref.typeSignature.baseClasses)
      .map(Reflection(ctx, bd, _))
  }
}


case class DeclarationReflection(
                                fullName: String,
                                kind: String,
                                position: Position,
                                isImplicit: Boolean,
                                baseClasses: List[DeclarationReflection],
                                typeSignature: u.Type,
                                paramLists: List[List[u.Symbol]],
                                returnType: u.Type
                                )

object DeclarationReflection{
  def apply(ctx: ReflectiveCtx, pos: Position, denot: Denotation, sym: u.Symbol): DeclarationReflection =
    DeclarationReflection(
      fullName = sym.fullName,
      kind = ctx.getKind(denot),
      position = pos,
      isImplicit = denot.isImplicit,
      baseClasses = baseClasses(ctx, sym),
      typeSignature = sym.typeSignature,
      paramLists = ctx.paramLists(sym),
      returnType = ctx.returnType(sym)
    )
  def baseClasses(ctx: ReflectiveCtx, symbol: u.Symbol): List[DeclarationReflection] = ???
}

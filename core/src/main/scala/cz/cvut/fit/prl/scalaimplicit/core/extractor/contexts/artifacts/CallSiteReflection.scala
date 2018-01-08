package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import org.langmeta.inputs.Position
import org.langmeta.semanticdb.Denotation

import scala.reflect.runtime.{universe => u}

case class ReflectiveTArg(fullName: String, args: Seq[ReflectiveTArg])
object ReflectiveTArg {
  def apply(ctx: ReflectiveCtx, targ: TArg): ReflectiveTArg =
    new ReflectiveTArg(
      fullName = ctx.findReflectSymbol(targ.symbol).fullName,
      args = targ.args.map(ReflectiveTArg(ctx, _))
    )
  def apply(tpe: u.Type): ReflectiveTArg = ReflectiveTArg(
    fullName = tpe.typeSymbol.fullName,
    args = tpe.typeArgs.map(ReflectiveTArg(_))
  )
}
case class CallSiteReflection(originalSymbol: QualifiedSymbol,
                              isImplicit: Boolean,
                              fullName: String,
                              kind: String,
                              params: Seq[Param],
                              typeArguments: Seq[ReflectiveTArg],
                              pos: Position,
                              code: String,
                              declaration: DeclarationReflection,
                              typeSignature: u.Type,
                              paramLists: List[List[u.Symbol]],
                              returnType: u.Type)
    extends Param
object CallSiteReflection {
  def apply(ctx: ReflectiveCtx,
            bd: BreakDown,
            ref: u.Symbol): CallSiteReflection =
    new CallSiteReflection(
      originalSymbol = bd.symbol,
      isImplicit = ref.isImplicit,
      fullName = ref.fullName,
      kind = ctx.getReflectiveKind(ref),
      pos = bd.pos,
      declaration = DeclarationReflection(ctx, Position.None, ref),
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
            ref: u.Symbol): CallSiteReflection =
    new CallSiteReflection(
      originalSymbol = bd.symbol,
      isImplicit = den.isImplicit,
      fullName = ref.fullName,
      kind = ctx.getKind(den),
      pos = bd.pos,
      declaration = DeclarationReflection(ctx, Position.None, ref),
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

}

case class ParentReflection(
    fullName: String,
    kind: String,
    declaration: DeclarationReflection,
    typeArguments: Seq[ReflectiveTArg]
)

object ParentReflection {
  def apply(ctx: ReflectiveCtx, pos: Position, tpe: u.Type): ParentReflection = {
    val sym = tpe.typeSymbol
    ParentReflection(
      fullName = sym.fullName,
      kind = ctx.getReflectiveKind(sym),
      declaration = DeclarationReflection(ctx, pos, sym),
      typeArguments = tpe.typeArgs.map(ReflectiveTArg(_))
    )
  }
}

case class DeclarationReflection(
    fullName: String,
    kind: String,
    position: Position,
    isImplicit: Boolean,
    baseClasses: List[ParentReflection],
    typeSignature: u.Type,
    paramLists: List[List[u.Symbol]],
    returnType: u.Type
)

object DeclarationReflection {
  def apply(ctx: ReflectiveCtx,
            pos: Position,
            sym: u.Symbol): DeclarationReflection =
    DeclarationReflection(
      fullName = sym.fullName,
      kind = ctx.getReflectiveKind(sym),
      position = pos,
      isImplicit = sym.isImplicit,
      baseClasses = baseClasses(ctx, pos, sym),
      typeSignature = sym.typeSignature,
      paramLists = ctx.paramLists(sym),
      returnType = ctx.returnType(sym)
    )

  def baseClasses(ctx: ReflectiveCtx,
                  pos: Position,
                  ref: u.Symbol): List[ParentReflection] = {
    ctx
      .firstLevelBaseClasses(ref.typeSignature.baseClasses)
      .map(ref.typeSignature.baseType(_))
      .map(ParentReflection(ctx, pos, _))
  }
}

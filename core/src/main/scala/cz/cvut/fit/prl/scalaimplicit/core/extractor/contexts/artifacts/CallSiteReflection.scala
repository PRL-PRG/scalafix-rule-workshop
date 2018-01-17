package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
}
import org.langmeta.inputs.Position
import org.langmeta.semanticdb.Denotation

import scala.reflect.runtime.{universe => u}

case class CallSiteReflection(originalSymbol: QualifiedSymbol,
                              reflectiveSymbol: u.Symbol,
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
            ref: u.Symbol,
            origins: SyntheticOrigins): CallSiteReflection =
    new CallSiteReflection(
      originalSymbol = bd.symbol,
      reflectiveSymbol = ref,
      isImplicit = ref.isImplicit,
      fullName = ref.fullName,
      kind = ReflectiveCtx.getReflectiveKind(ref),
      pos = bd.pos,
      declaration = DeclarationReflection(ctx, Position.None, ref, None),
      code = bd.code,
      params = bd.args.map(ctx.reflectiveParam(_, origins.paramList)),
      typeArguments = bd.targs.map(ReflectiveTArg(ctx, _, origins.application)),
      typeSignature = ref.typeSignature,
      paramLists = ReflectiveCtx.paramLists(ref),
      returnType = ReflectiveCtx.returnType(ref)
    )

  def apply(ctx: ReflectiveCtx,
            bd: BreakDown,
            den: Denotation,
            ref: u.Symbol,
            origins: SyntheticOrigins): CallSiteReflection =
    new CallSiteReflection(
      originalSymbol = bd.symbol,
      reflectiveSymbol = ref,
      isImplicit = den.isImplicit,
      fullName = ref.fullName,
      kind = SemanticCtx.getKind(den),
      pos = bd.pos,
      declaration = DeclarationReflection(ctx, Position.None, ref, Some(den)),
      code = bd.code,
      params = bd.args.map(ctx.reflectiveParam(_, origins.paramList)),
      typeArguments = bd.targs.map(ReflectiveTArg(ctx, _, origins.application)),
      typeSignature = ref.typeSignature,
      paramLists = ReflectiveCtx.paramLists(ref),
      returnType = ReflectiveCtx.returnType(ref)
    )
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
      kind = ReflectiveCtx.getReflectiveKind(sym),
      declaration = DeclarationReflection(ctx, pos, sym, None),
      typeArguments = tpe.typeArgs.map(ReflectiveTArg(_))
    )
  }
}

case class DeclarationReflection(
    sym: u.Symbol,
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
            sym: u.Symbol,
            denot: Option[Denotation]): DeclarationReflection =
    DeclarationReflection(
      sym = sym,
      fullName = sym.fullName,
      kind = denot match {
        case Some(d) => SemanticCtx.getKind(d)
        case None => ReflectiveCtx.getReflectiveKind(sym)
      },
      position = pos,
      isImplicit = denot match {
        case Some(d) => d.isImplicit
        case None => sym.isImplicit
      },
      baseClasses = baseClasses(ctx, pos, sym),
      typeSignature = sym.typeSignature,
      paramLists = ReflectiveCtx.paramLists(sym),
      returnType = ReflectiveCtx.returnType(sym)
    )

  def baseClasses(ctx: ReflectiveCtx,
                  pos: Position,
                  ref: u.Symbol): List[ParentReflection] = {
    ReflectiveCtx
      .firstLevelBaseClasses(ref.typeSignature.baseClasses)
      .map(ref.typeSignature.baseType(_))
      .map(ParentReflection(ctx, pos, _))
  }
}

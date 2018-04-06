package cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{ReflectiveCtx, SemanticCtx}
import org.langmeta.inputs.Position
import org.langmeta.semanticdb.Denotation

import scala.reflect.runtime.{universe => u}

/**
  * Intermediate representation that deals with incomplete information
  * (e.g. a missing Denotation) and gathers the information needed to
  * call the factories later.
  *
  * It could arguably be merged with the Factories, but that would make
  * the factories very complicated.
  */
/**
  * The reflection for either an implicit CallSite or an ImplicitArgument in a call site.
  */
case class ImplicitReflection(originalSymbol: QualifiedSymbol,
                              reflectiveSymbol: u.Symbol,
                              fullName: String,
                              pos: Position,
                              code: String,
                              declaration: DeclarationReflection,
                              typeArguments: Seq[ReflectiveTArg],
                              args: Seq[Param])
    extends Param
object ImplicitReflection {
  def apply(ctx: ReflectiveCtx,
            bd: BreakDown,
            den: Option[Denotation],
            ref: u.Symbol,
            origins: SyntheticOrigins): ImplicitReflection =
    new ImplicitReflection(
      originalSymbol = bd.symbol,
      reflectiveSymbol = ref,
      fullName = ref.fullName,
      pos = bd.pos,
      declaration = DeclarationReflection(ctx, Position.None, ref, den),
      code = bd.code,
      args = bd.args.map(ctx.reflectOnArg(_, origins.paramList)),
      typeArguments = bd.targs.map(ReflectiveTArg(ctx, _, origins.application))
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

  def createFromLocal(pos: Position, denot: Denotation) =
    new DeclarationReflection(
      sym = u.NoSymbol,
      fullName = s"_local_${denot.name}${denot.signature}",
      kind = SemanticCtx.getKind(denot),
      position = pos,
      isImplicit = denot.isImplicit,
      baseClasses = List(),
      typeSignature = u.NoType,
      paramLists = List(List()),
      returnType = u.NoType
    )

  def apply(ctx: ReflectiveCtx, bd: DefnBreakdown): DeclarationReflection = {
    bd.sym match {
      case Some(s) => DeclarationReflection(ctx, bd.pos, s, bd.den)
      case None => createFromLocal(bd.pos, bd.den.get)
    }
  }

  def baseClasses(ctx: ReflectiveCtx,
                  pos: Position,
                  ref: u.Symbol): List[ParentReflection] = {
    if (!ref.getClass.getName.contains("SynchronizedSymbol")) {
      throw new RuntimeException("asdfasdfasd")
    }
    ReflectiveCtx
      .firstLevelBaseClasses(ref.typeSignature.baseClasses)
      .map(ref.typeSignature.baseType(_))
      .map(ParentReflection(ctx, pos, _))
  }
}

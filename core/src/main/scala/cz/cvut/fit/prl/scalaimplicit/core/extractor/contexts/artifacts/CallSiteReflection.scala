package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
}
import org.langmeta.inputs.Position
import org.langmeta.semanticdb.Denotation

import scala.meta.{Term, Tree, Type}
import scala.{meta => m}
import scala.reflect.runtime.{universe => u}

case class ReflectiveTArg(fullName: String, args: Seq[ReflectiveTArg])
object ReflectiveTArg {
  def processType(ctx: ReflectiveCtx, targ: Type)(
      implicit finder: Type => m.Symbol): ReflectiveTArg = targ match {
    case t: Type.Apply => {
      val pt = processType(ctx, t.tpe)
      val targs = t.args.map(processType(ctx, _))
      ReflectiveTArg(pt.fullName, targs)
    }
    case t: Type.Name => {
      val symbol = ctx.findReflectSymbol(finder(t)).fullName
      ReflectiveTArg(symbol, Seq())
    }
  }

  def apply(ctx: ReflectiveCtx,
            targ: m.Type,
            synthSource: Option[m.Synthetic]): ReflectiveTArg = {
    def typeFinder(t: Type): m.Symbol = {
      synthSource match {
        // The application came from a synthetic
        case Some(synth) => {
          synth.names.find(_.position.end == t.pos.end) match {
            case Some(name) => name.symbol
            case None =>
              throw new MatchError(s"No name found in types for ${targ}")
          }
        }
        // We are looking for data from an application
        // that is in the source, so there is no synthetic
        case None => {
          ctx.names.find(_.position.end == t.pos.end).get.symbol
        }
      }
    }
    processType(ctx, targ)(typeFinder)
  }
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
            ref: u.Symbol,
            origin: Option[m.Synthetic]): CallSiteReflection =
    new CallSiteReflection(
      originalSymbol = bd.symbol,
      isImplicit = ref.isImplicit,
      fullName = ref.fullName,
      kind = ctx.getReflectiveKind(ref),
      pos = bd.pos,
      declaration = DeclarationReflection(ctx, Position.None, ref),
      code = bd.code,
      params = bd.args.map(ctx.reflectiveParam(_, origin)),
      typeArguments = bd.targs.map(ReflectiveTArg(ctx, _, origin)),
      typeSignature = ref.typeSignature,
      paramLists = ctx.paramLists(ref),
      returnType = ctx.returnType(ref)
    )

  def apply(ctx: ReflectiveCtx,
            bd: BreakDown,
            den: Denotation,
            ref: u.Symbol,
            origin: Option[m.Synthetic]): CallSiteReflection =
    new CallSiteReflection(
      originalSymbol = bd.symbol,
      isImplicit = den.isImplicit,
      fullName = ref.fullName,
      kind = SemanticCtx.getKind(den),
      pos = bd.pos,
      declaration = DeclarationReflection(ctx, Position.None, ref),
      code = bd.code,
      params = bd.args.map(ctx.reflectiveParam(_, origin)),
      typeArguments = bd.targs.map(ReflectiveTArg(ctx, _, origin)),
      typeSignature = ref.typeSignature,
      paramLists = ctx.paramLists(ref),
      returnType = ctx.returnType(ref)
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

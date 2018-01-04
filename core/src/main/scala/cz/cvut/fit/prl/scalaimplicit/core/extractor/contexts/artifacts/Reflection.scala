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
                      hasLocation: Boolean,
                      baseClasses: List[Reflection],
                      params: Seq[Param],
                      typeArguments: Seq[ReflectiveTArg],
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
      hasLocation = false,
      baseClasses = baseClasses(ctx, bd, ref),
      pos = bd.pos,
      code = bd.code,
      params = bd.params.map(reflectiveParam(ctx, _)),
      typeArguments = bd.typeParams.map(ReflectiveTArg(ctx, _)),
      typeSignature = ref.typeSignature,
      paramLists = paramLists(ref),
      returnType = returnType(ref)
    )

  def apply(ctx: ReflectiveCtx,
            bd: BreakDown,
            den: Denotation,
            ref: u.Symbol): Reflection =
    new Reflection(
      originalSymbol = bd.symbol,
      isImplicit = den.isImplicit,
      fullName = den.name,
      kind = ctx.getKind(den),
      hasLocation = true,
      baseClasses = baseClasses(ctx, bd, ref),
      pos = bd.pos,
      code = bd.code,
      params = bd.params.map(reflectiveParam(ctx, _)),
      typeArguments = bd.typeParams.map(ReflectiveTArg(ctx, _)),
      typeSignature = ref.typeSignature,
      paramLists = paramLists(ref),
      returnType = returnType(ref)
    )

  def returnType(ref: u.Symbol) = {
    ref match {
      case r if r.isMethod => r.asMethod.returnType
      case r => r.typeSignature
    }
  }

  def paramLists(ref: u.Symbol) = {
    ref match {
      case r if r.isMethod => r.asMethod.paramLists
      case _ => List()
    }
  }

  def reflectiveParam(ctx: ReflectiveCtx, param: Param): Param = {
    param match {
      case bd: BreakDown => ctx.findReflection(bd)
      case p: Param => p
    }
  }

  def baseClasses(ctx: ReflectiveCtx,
                  bd: BreakDown,
                  ref: u.Symbol): List[Reflection] = {
    def firstLevelBaseClasses(baseClasses: List[u.Symbol]): List[u.Symbol] = {
      // Take the tail because the first one is the self definition
      // Remove the classes that are parents of some class in bases
      baseClasses match {
        case bases if bases.isEmpty => List()
        case bases =>
          bases.tail.filterNot(cls =>
            bases.tail.exists(_.typeSignature.baseClasses.tail.contains(cls)))
      }
    }
    firstLevelBaseClasses(ref.typeSignature.baseClasses)
      .map(Reflection(ctx, bd, _))
  }
}

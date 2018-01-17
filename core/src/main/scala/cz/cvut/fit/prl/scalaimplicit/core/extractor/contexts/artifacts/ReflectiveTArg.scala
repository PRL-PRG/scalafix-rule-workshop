package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.{
  ReflectiveCtx,
  SemanticCtx
}

import scala.meta.{Tree, Type}
import scala.{meta => m}
import scala.reflect.runtime.{universe => u}

case class ReflectiveTArg(fullName: String, args: Seq[ReflectiveTArg] = Seq())

object ReflectiveTArg {

  def symbolName(ctx: ReflectiveCtx, t: Tree, symbol: m.Symbol): String = {
    def handleLocalTypeReference(ltr: m.Symbol.Global): String = {
      s"${ReflectiveCtx.Cleaners.cleanOwner(
        ltr.productElement(0).asInstanceOf[m.Symbol.Global].productElement(0).toString)}.${ltr.syntax}"
    }

    symbol match {
      case s: m.Symbol.Local => s"_local_.${t.syntax}"
      case s: m.Symbol.Global => {
        if (SemanticCtx.isLocalTypeReference(s)) handleLocalTypeReference(s)
        else ctx.findReflectSymbol(s).fullName
      }
      case s => ??? // This is to prevent a warning, for incomplete matching
      // This warning malforms the semanticdb file.
    }
  }

  def processType(ctx: ReflectiveCtx, targ: Type)(
      implicit finder: Type => m.Symbol): ReflectiveTArg = targ match {
    case t: Type.Apply => {
      val pt = processType(ctx, t.tpe)
      val targs = t.args.map(processType(ctx, _))
      ReflectiveTArg(pt.fullName, targs)
    }
    case t: Type.Name => {
      val symbol = symbolName(ctx, t, finder(t))
      ReflectiveTArg(symbol)
    }
    case t: Type.Select => ReflectiveTArg(t.syntax)
    case t: Type.Refine => ReflectiveTArg(t.toString)
    case t: Type.Function => ReflectiveTArg(t.toString)
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

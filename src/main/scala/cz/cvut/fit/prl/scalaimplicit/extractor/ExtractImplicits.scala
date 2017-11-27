package cz.cvut.fit.prl.scalaimplicit.extractor

import cz.cvut.fit.prl.scalaimplicit.extractor.Serializables.{
  Apply,
  DeclaredImplicit,
  FunApplyWithImplicitParam,
  ImplicitParam
}
import org.langmeta.inputs.Input

import scala.meta._

final case class Result(params: Set[ImplicitParam],
                        funs: Seq[Apply],
                        links: Set[FunApplyWithImplicitParam],
                        implicits: Set[DeclaredImplicit])
object Result {
  val Empty = Result(Set(), Seq(), Set(), Set())
}

object ExtractImplicits extends (SemanticCtx => Result) {

  final case class LinkPair(param: ImplicitParam, fun: Apply)

  def apply(ctx: SemanticCtx): Result = {
    val file: String = ctx.input match {
      case Input.VirtualFile(path, _) => path
      case Input.File(path, _) => path.toString
      case _ => ""
    }

    lazy val syntheticImplicits =
      for {
        syn <- ctx.index.synthetics
        name <- syn.names
        symbol = name.symbol
        den <- ctx.denotation(symbol) if den.isImplicit
      } yield {
        syn -> Serializables.createImplicitParam(ctx, symbol, den)
      }

    lazy val syntheticApplies =
      ctx.index.synthetics.filter(_.names.exists(_.toString() == "apply"))

    lazy val paramsFuns =
      for {
        app <- ctx.tree collect {
          case x: Term.Apply =>
            Serializables.AppTerm(x, x.args.size, x.fun.pos.end)
          case x: Term.Name => Serializables.AppTerm(x, 0, x.pos.end)
        }
        param <- syntheticImplicits collect {
          case (syn, den) if syn.position.end == app.term.pos.end => den
        }
        syntheticApply = syntheticApplies find { x =>
          x.position.end >= app.term.pos.start && x.position.end <= app.term.pos.end
        }
      } yield {
        syntheticApply match {
          case Some(synth) =>
            LinkPair(param,
                     Serializables.createSyntheticApplication(ctx,
                                                              synth,
                                                              file,
                                                              app.params))
          case None =>
            LinkPair(param,
                     Serializables.createFunctionApplication(ctx, app, file))
        }
      }

    val params = paramsFuns.groupBy(_.param).keys.toSet
    val funs = paramsFuns.groupBy(_.fun).keys.toSeq
    val links =
      paramsFuns.map(x => Serializables.createLink(x.param, x.fun)).toSet

    lazy val declaredImplicits =
      for {
        name <- ctx.index.names if name.isDefinition
        den <- ctx.denotation(name.symbol) if den.isImplicit
      } yield {
        Serializables.createDeclaredImplicit(ctx, name, den, file)
      }

    Result(params, funs, links, declaredImplicits.toSet)
  }
}

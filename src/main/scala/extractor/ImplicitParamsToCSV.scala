package extractor

import org.langmeta.inputs.Input
import scala.meta._

object ImplicitParamsToCSV {

  def apply(): Unit = {
    SemanticdbFileWalker.run { ctx =>
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
          syn -> ImplicitParam(ctx, symbol, den)
        }

      lazy val syntheticApplies = ctx.index.synthetics.filter(_.names.exists(_.toString() == "apply"))

      lazy val allApps = ctx.tree collect {
        case x: Term.Apply => AppTerm(x, x.args.size, x.fun.pos.end)
        case x: Term.Name => AppTerm(x, 0, x.pos.end)
      }

      lazy val paramsFuns =
        for {
          app <- allApps
          param <- syntheticImplicits collect {
            case (syn, den) if syn.position.end == app.term.pos.end => den
          }
          syntheticApply = syntheticApplies find {
            x => x.position.end >= app.term.pos.start && x.position.end <= app.term.pos.end
          }
        } yield {
          syntheticApply match {
            case Some(synth) => FunApplyWithImplicitParam(SyntheticApply(ctx, synth, file, app.params), param)
            case None => FunApplyWithImplicitParam(FunApply(ctx, app, file), param)
          }
        }

      val params = paramsFuns.groupBy(_.param).keys.toSet
      val funs = paramsFuns.groupBy(_.fun).keys

      CSV.writeCSV(params, s"${ctx.projectPath}/params.csv")
      CSV.writeCSV(funs, s"${ctx.projectPath}/funs.csv")
      CSV.writeCSV(paramsFuns, s"${ctx.projectPath}/params-funs.csv")
    }

  }
}

package extractor

import java.util.concurrent.ConcurrentLinkedQueue

import org.langmeta.inputs.Input

import scala.meta._

// TODO: this should be called like ExtractImplicits
object ImplicitParamsToCSV {

  final case class Result(
     params: Set[ImplicitParam],
     funs: Seq[FunApply],
     links: Set[FunApplyWithImplicitParam],
     implicits: Set[DeclaredImplicit])

  /**
    * Function that, given a queue of `results`, condenses them into a single map,
    * performing project-wide modifications (e.g. Remove project-wide duplicates on params)
    *
    * @param results Results from running the analysis in a single project concurrently. Given as a list of tuples (filename, case classes to dump)
    * @return A map of (filename -> data), to be dumped by the CSV class
    */
  def mergeResults(results: ConcurrentLinkedQueue[(String, Iterable[CSV.Serializable])]): Map[String, Iterable[CSV.Serializable]] = {
    import scala.collection.JavaConverters._

    // Condense the queue into a map of Strings to Lists of case classes
    val rawResults: Map[String, Iterable[CSV.Serializable]] = results
      .asScala
      .groupBy(_._1)
      .map {case (k, v) =>
        (k, v.flatMap(_._2))}

    // Apply transformations to the map
    rawResults.map{
      // Remove duplicates, where duplicates are two ImplicitParameters with the same id fields
      // we can't use toSet because it compares hashes, not ids, and the ctx field of the case classes
      // is not the same
      case ("params.csv", implicits) => ("params.csv", implicits
        .groupBy(_.id)
        .map{case (k, v) => v.head}
      )
      case (k, v) => (k, v)
    }
  }

  def process(db: Database): Result = {
    // TODO: the code from apply should move here and then it should be called apply
    Result(Set(), Seq(), Set(), Set())
  }

  def apply(walker: SemanticDBWalker): Map[String, Iterable[CSV.Serializable]] = {
    mergeResults( walker.run { ctx =>
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

      lazy val paramsFuns =
        for {
          app <- ctx.tree collect {
            case x: Term.Apply => AppTerm(x, x.args.size, x.fun.pos.end)
            case x: Term.Name => AppTerm(x, 0, x.pos.end)
          }
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

      lazy val declaredImplicits =
        for {
          name <- ctx.index.names
          den <- ctx.denotation(name.symbol) if den.isImplicit
        } yield {
          DeclaredImplicit(ctx, name, den, file)
        }

      val res = List(
        "params.csv" -> params,
        "funs.csv" -> funs,
        "params-funs.csv" -> paramsFuns,
        "declared-implicits.csv" -> declaredImplicits
      )
      res
    })
  }
}

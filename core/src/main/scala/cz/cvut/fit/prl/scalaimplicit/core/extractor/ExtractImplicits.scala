package cz.cvut.fit.prl.scalaimplicit.core.extractor

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  CallSite,
  Declaration
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.decomposers.{
  DefnDecomposer,
  InitDecomposer,
  TermDecomposer
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Factories

import scala.meta._
import scala.reflect.runtime.{universe => u}
import scala.util.{Failure, Success, Try}

object Queries {
  def hasImplicits(ctx: ReflectiveCtx, breakdown: DefnBreakdown): Boolean = {
    def hasImplicitsRef(ref: u.Symbol): Boolean = {
      ref.isImplicit || ref.typeSignature.paramLists.exists(ls =>
        ls.exists(param => hasImplicitsRef(param)))
    }

    def hasImplicitsDen(den: Option[Denotation]): Boolean = {
      den match {
        case Some(d) =>
          d.isImplicit || d.names.exists(n =>
            hasImplicitsDen(ctx.denotation(n.symbol)))
        case None => false
      }
    }

    hasImplicitsDen(breakdown.den) || (breakdown.sym.isDefined && hasImplicitsRef(
      breakdown.sym.get))
  }

  def breakDownSynthetic(ctx: SemanticCtx,
                         synth: Synthetic): SyntheticBreakdown = {

    def parse(text: String): Term = text.parse[Term].get

    def finder(tree: Tree): QualifiedSymbol = {
      synth.names.find(_.position.end == tree.pos.end) match {
        // Filter out the _star_ names
        // TODO: I don't think this case is relevant anymore, see processParamList()
        case Some(n) if n.symbol.syntax.contains("_star_") =>
          QualifiedSymbol.Empty
        case Some(name) =>
          QualifiedSymbol(Some(name.symbol), isSynthetic = true)
        case None => QualifiedSymbol.Empty
      }
    }

    val processedSynthetic = {
      val bd = TermDecomposer(parse(synth.text), finder).copy(
        pos = synth.position
      )
      SyntheticBreakdown(
        breakDown = bd,
        SyntheticOrigins(
          application = if (bd.symbol.app.isDefined) Some(synth) else None,
          paramList = Some(synth)
        )
      )
    }
    val res = processedSynthetic.breakDown.symbol.app match {
      case Some(app) => processedSynthetic
      case None => {
        val matchedApplication = findApplication(ctx, synth)
        //assertWeCanEraseParams(matchedApplication)
        matchedApplication.copy(
          matchedApplication.breakDown.copy(
            args = processedSynthetic.breakDown.args
          ),
          matchedApplication.origins.copy(
            paramList = processedSynthetic.origins.paramList
          )
        )
      }
    }
    assert(
      res.breakDown.symbol.app.isDefined,
      s"Couldn't find an application for synthetic ${synth.text}"
    )
    res
  }

  /**
    * Find applications for the synthetics that don't have them (that is, pure parameter lists)
    * For that, we try to look for a fitting `apply` synthetic. If we don't find one, we look in
    * the source code and try to match there.
    *
    * In both cases, we match by position, since parameter lists are inserted at the end of calls.
    *
    * We assume that there is exactly one symbol at the position of the synthetic.
    */
  def findApplication(ctx: SemanticCtx, synth: Synthetic): SyntheticBreakdown = {

    def breakdownTree(term: Tree): BreakDown = {
      def finder(t: Tree): QualifiedSymbol = {
        val sym = ctx.symbol(t)
        // A symbol from the tree will never be synthetic
        t match {
          // Special case: https://github.com/PRL-PRG/scalafix-rule-workshop/issues/39
          case tree: Term.Name
              if sym.isDefined &&
                sym.get.isInstanceOf[Symbol.Local] =>
            QualifiedSymbol(ctx.unrecurse(tree), isSynthetic = false)
          case tree =>
            QualifiedSymbol(
              Some(
                sym.getOrElse(
                  Symbol(ctx.qualifiedName(t.asInstanceOf[Term])))),
              isSynthetic = false
            )
        }
      }

      term match {
        case t: Init => InitDecomposer(term.asInstanceOf[Init], finder)
        case t: Term => TermDecomposer(term.asInstanceOf[Term], finder)
      }
    }

    ctx.syntheticApplication(synth.position.end) match {
      // There is a synthetic application that matches
      case Some(syntheticApply) => breakDownSynthetic(ctx, syntheticApply)
      // Parse from the tree itself
      case None =>
        SyntheticBreakdown(
          breakdownTree(ctx
            .inSourceCallSite(synth.position.end)
            .getOrElse {
              throw new RuntimeException(
                s"No application found in source for ${synth.text}@${synth.position.endLine}:${synth.position.endColumn}")
            })
        )
    }
  }

  def getDefn(ctx: ReflectiveCtx, tree: Tree): Seq[DefnBreakdown] = {
    def finder(t: Tree): Symbol = ctx.symbol(t).get
    DefnDecomposer(ctx, tree)(finder)
  }
}

case class ExtractionResult(callSites: Seq[CallSite],
                            declarations: Set[Declaration])
object ExtractionResult {
  val Empty = ExtractionResult(Seq(), Set())
}
object ReflectExtract extends (ReflectiveCtx => ExtractionResult) {

  def extractCallSites(ctx: ReflectiveCtx): Seq[CallSite] =
    ctx.syntheticsWithImplicits
      .map(
        syn =>
          Try(
            Factories.createCallSite(ctx,
                                     ctx.reflectOnBreakdown(
                                       Queries.breakDownSynthetic(ctx, syn)
                                     ))
        )
      )
      .reportAndExtract("CallSite")
  /* Replaced with instance-by-instance processing to be able to log exceptions easily
      .map(Queries.breakDownSynthetic(ctx, _))
      .map(ctx.reflectOnBreakdown)
      .map(Factories.createCallSite(ctx, _))
   */

  private implicit class TryCollection[A](from: Seq[Try[A]]) {
    def reportAndExtract(header: String): Seq[A] =
      from
        .map {
          case Failure(t) => ErrorCollection().report(header, t); Failure(t)
          case t => t
        }
        .collect {
          case Success(t) => t
        }
  }

  def extractDeclarations(ctx: ReflectiveCtx): Set[Declaration] =
    ctx.inSourceDefinitions
      .map(
        t =>
          Try(
            Queries
              .getDefn(ctx, t)
        )
      )
      .reportAndExtract("Definition")
      .flatten
      .map(d =>
        Factories.createDeclaration(ctx, DeclarationReflection(ctx, d)))
      .toSet

  /*
      .flatMap(Queries.getDefn(ctx, _))
      //.filter(Queries.hasImplicits(ctx, _))
      .map(DeclarationReflection(ctx, _))
      .map(Factories.createDeclaration(ctx, _))
      .toSet
   */

  def apply(ctx: ReflectiveCtx): ExtractionResult = {
    ExtractionResult(extractCallSites(ctx), extractDeclarations(ctx))
  }
}

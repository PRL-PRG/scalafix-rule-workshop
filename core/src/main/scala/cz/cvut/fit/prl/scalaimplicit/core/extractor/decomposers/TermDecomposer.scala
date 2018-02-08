package cz.cvut.fit.prl.scalaimplicit.core.extractor.decomposers

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SemanticCtx
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts.{
  BreakDown,
  Param,
  QualifiedSymbol,
  RawCode
}

import scala.meta.{Lit, Name, Term, Tree}

/**
  * Common interface for objects that decompose terms
  *
  * Walking a term tree to form a useful function
  * is a tedious process. This has to be done twice -
  * Once on the synthetics to examine the synthetic itself,
  * and another one once we have matched the synthetics with their missing applications.
  * This means that the only difference is in how they find symbols
  * (one finds them in the synthetic, the other in the code),
  * which why is this accepts a finder function.
  *
  * The accepted function is implicit for convenience, since breakDown and
  * processType are recursive and implicit functions simplify the calls.
  */
object TermDecomposer {
  def apply(tree: Term, finder: Tree => QualifiedSymbol): BreakDown = {
    breakDown(tree)(finder)
  }

  def processParamList(params: Seq[Term])(
      implicit finder: Tree => QualifiedSymbol): Seq[Param] = {
    def getParameter(term: Tree): Param = term match {
      case t: Term.Assign => {
        RawCode(t.syntax, t.pos)
      }
      case t: Term.Block => {
        // A block inside the synthetic (e.g. `nested.this.a2c(*)({((a: A) => nested.this.a2b(a))})`)
        // We assume it has only one stat inside, and that it is a Term.
        // If it has more than one stat, we just regard it as a parameter
        if (t.stats.size == 1) {
          assert(t.stats.forall(_.isInstanceOf[Term]),
                 s"Stat (${t.stats.head}) from block ${t} is not a term")
          getParameter(t.stats.head.asInstanceOf[Term])
        } else {
          RawCode(t.syntax, t.pos)
        }
      }
      case t: Term.Function => {
        // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
        // If the function appears in a parameter list, it is safe to consider it
        // as raw code, since implicit defs passed as implicit parameters will be passed in blocks
        // (See `case t: Term.Block`)
        t.body match {
          case application if SemanticCtx.isApplication(application) =>
            breakDown(application)
          case body => RawCode(t.syntax, t.pos)
        }
      }
      case t: Term.PartialFunction => {
        // A generated partial function application (e.g. `{ case (_, v) => v.head }`)
        RawCode(t.syntax, t.pos)
      }
      case t: Term.Ascribe => {
        // Type ascriptions: `((ClassTag.apply[String](classOf[java.lang.String])): ClassTag[String])`
        val app = getParameter(t.expr)
        app match {
          case app: BreakDown =>
            app.copy(targs = Seq(t.tpe), pos = t.pos, code = t.syntax)
          case app: Param => app
        }
      }
      case t: Term.Name => RawCode(t.syntax, t.pos)
      case t: Term.Placeholder => RawCode(t.syntax, t.pos)
      case t: Term.Interpolate => RawCode(t.syntax, t.pos)
      case t: Term.Repeated => RawCode(t.syntax, t.pos)
      case t: Term.Throw => RawCode(t.syntax, t.pos)
      case t: Lit => RawCode(t.syntax, t.pos)
      case t: Term => {
        val bd = breakDown(t)
        bd.symbol.app match {
          case Some(s) => bd
          case None => RawCode(t.syntax, t.pos)
        }
      }
      case t => RawCode(t.syntax, t.pos)
    }
    params
      .filterNot(x => {
        x.toString() == "*"
      })
      .map(getParameter)
  }

  private def breakDown(tree: Term)(
      implicit finder: Tree => QualifiedSymbol): BreakDown = {

    tree match {
      case t: Term.Apply => {
        // Anything with a parameter list (`hello(*)`, `hello[String](*)`, `hello[String](*)(stringConverter)`...)
        val bd = breakDown(t.fun)
        bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
      }
      case t: Term.ApplyType => {
        // An application with type parameters but no parameter list
        // examples: `test.this.JsonWriter[Seq[Student]]`
        val bd = breakDown(t.fun)
        bd.copy(targs = t.targs, pos = t.pos, code = t.syntax)
      }
      case t: Term.ApplyInfix => {
        // Infix function applications (e.g. `list map f1`).
        // We take the operation and the ''implicit'' parameters
        val bd = breakDown(t.op)
        bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
      }
      case t: Term.ApplyUnary => {
        // Unary functions
        val bd = breakDown(t.op)
        bd.copy(args = processParamList(Seq(t.arg)),
                pos = t.pos,
                code = t.syntax)
      }
      case t: Term.Select => {
        // Does not have parameters (otherwise it would be a Term.Apply) or type parameters (Term.ApplyType)
        // examples: `test.this.JsonWriter`
        breakDown(t.name).copy(pos = t.pos, code = t.syntax)
      }
      case t: Term.Name => {
        // Plain name of the symbol we want (e.g. in `test.this.JsonWriter` -> `"JsonWriter"`)
        val app = finder(t)
        BreakDown(app, Seq(), Seq(), t.pos)
      }
      case t: Term.New => {
        // Class constructor calls.
        // If the constructor is anonymous, we return the symbol of the class.
        // FIXME: This assumes that we won't find this in a synthetic
        val app = t.init.name match {
          case n: Name.Anonymous => finder(t.init.tpe)
        }
        BreakDown(app, Seq(), Seq(), pos = t.pos, code = t.syntax)
      }
      case t: Term.NewAnonymous => {
        // Anonymously constructed objects:
        // `new SnapshotFixtures { val buffer = // ... }`
        // We assume we have only one Init
        assert(t.templ.inits.size == 1,
               s"More than one init found for NewAnonymous ${t}")
        InitDecomposer(t.templ.inits.head, finder)
      }
      case t: Term.Function => {
        // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
        // We assume that the body is a single function call, as is the most typical in passing parameters
        // We also ignore the parameters for now, since they will appear in other call sites,
        // when the function gets executed
        breakDown(t.body)
      }
      case t: Term.Interpolate => {
        val bd = breakDown(t.prefix)
        bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
      }
      case t => {
        throw new MatchError(s"Unknown Term match ${t.structure}")
      }
    }
  }
}

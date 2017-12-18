package cz.cvut.fit.prl.scalaimplicit.macros

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite
import sbt.complete.Parsers

import scala.reflect.macros.blackbox.Context

object QueryEngineMacros {
  import scala.language.experimental.macros

  def qcs(pat: String, guard: String): (CallSite => Boolean) = macro qcs_impl

  def qcs_impl(c: Context)(
      pat: c.Tree,
      guard: c.Tree): c.Expr[Function1[CallSite, Boolean]] = {
    import c.universe._

    val Literal(Constant(patStr: String)) = pat
    def extractApply(fun: Function): Apply = {
      val vparams = fun.vparams.map(_.name)
      val body = fun.body.asInstanceOf[Apply]
      val liftedArgs = body.args.collect {
        case arg: Function => extractApply(arg)
        case arg: Ident if vparams.contains(arg.name) =>
          Ident(termNames.WILDCARD)
        case a: Ident if a.name != termNames.WILDCARD =>
          Bind(a.name, Ident(termNames.WILDCARD))
        case other => other
      }
      Apply(body.fun, liftedArgs)
    }
    val patRepr = extractApply(c.parse(patStr).asInstanceOf[Function])

    val Literal(Constant(guardStr: String)) = guard
    val guardRepr = guardStr match {
      case str if str.isEmpty => Literal(Constant(true))
      case str => c.parse(guardStr)
    }

    val caseDef = cq"$patRepr if $guardRepr => true"

    val testPattern =
      cq"""CallSite(ss,_,_,_,Declaration(xx,_,_,_,_,_),_,_) if ss != xx => true"""

    val fun = c.Expr[Function1[CallSite, Boolean]](q"""
         (cs: CallSite) => {
           cs match {
             case $caseDef
             case _ => false
           }
         }
       """)
    // Print the generated code for debugging
    //println(fun)
    fun
  }

}

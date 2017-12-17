package cz.cvut.fit.prl.scalaimplicit.macros

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite

import scala.reflect.macros.blackbox.Context

object QueryEngineMacros {
  import scala.language.experimental.macros

  def qcsm(queries: (String, String)*): (CallSite => Boolean) = macro qcsm_impl

  def qcsm_impl(c: Context)(queries: c.Expr[(String, String)]*)
    : c.Expr[Function1[CallSite, Boolean]] = {
    import c.universe._
    def getStrings(tree: c.Tree): (String, String) = {
      val Apply(_,
                List(Literal(Constant(one: String)),
                     Literal(Constant(other: String)))) = tree
      (one, other)
    }
    def getName(of: Option[String]): c.Tree = of match {
      case Some(s) => Literal(Constant(of.getOrElse("_")))
      case None => Ident(termNames.WILDCARD)
    }

    val m: Map[String, String] = queries.map(x => getStrings(x.tree)).toMap
    val cas =
      cq"""CallSite(
         ${getName(m.get("name"))},
         ${getName(m.get("code"))},
         ${getName(m.get("location"))},
         ${getName(m.get("isSynthetic"))},
         ${getName(m.get("declaration"))},
         ${getName(m.get("typeArguments"))},
         ${getName(m.get("implicitArguments"))}
         ) => true
       """

    // TODO Delete this when the macro is stable
    val mcas =
      cq"""CallSite("scala.Predef.implicitly", _, _, _, _, _, _) => true"""

    c.Expr[Function1[CallSite, Boolean]](q"""
       (cs: CallSite) => {
         val res = cs match {
          case $cas
          case _ => false
         }
         res
        }""")
  }
}

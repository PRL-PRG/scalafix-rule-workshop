package cz.cvut.fit.prl.scalaimplicit.macros

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite

import scala.reflect.macros.whitebox

object QueryEngineMacros {
  import scala.language.experimental.macros
  def cimpl(c: whitebox.Context)(cs: c.Expr[CallSite], q: c.Expr[String]): c.Tree = {
    import c.universe._
    val pattern = cq"$q => true"
    q"""$cs match {
      case ${pattern}
      case _ => false
    }"""
  }

  def coincides(cs: CallSite, q: String): Boolean = macro cimpl
}

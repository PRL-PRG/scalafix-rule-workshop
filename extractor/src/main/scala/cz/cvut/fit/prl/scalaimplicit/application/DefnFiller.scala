package cz.cvut.fit.prl.scalaimplicit.application

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ImplicitAnalysisResult,
  OrphanCallSites
}
import cz.cvut.fit.prl.scalaimplicit.schema._

object DefnFiller extends (ImplicitAnalysisResult => ImplicitAnalysisResult) {
  def findDeclOrReport(target: {
    def declaration: Declaration; def name: String
  }, definitions: Set[Declaration]): Declaration =
    definitions
      .find(_.name == target.name)
      .getOrElse({
        OrphanCallSites().report("Orphan CallSite",
                                 s"Declaration not found for {$target.name}")
        target.declaration
      })

  def processArgList(args: Seq[Argument],
                     definitions: Set[Declaration]): Seq[Argument] = {
    args.map { arg =>
      arg.copy(
        info = arg.info.map(
          _.copy(
            declaration = findDeclOrReport(arg.info.get, definitions),
            arguments = processArgList(arg.info.get.arguments, definitions)
          )))
    }
  }

  def apply(result: ImplicitAnalysisResult): ImplicitAnalysisResult = {
    val defns = result.declarations
    val nres = result.copy(
      declarations = defns,
      callSites = result.callSites.map(
        cs =>
          cs.copy(
            declaration = cs.declaration.copy(
              location = findDeclOrReport(cs, defns).location),
            implicitArguments = processArgList(cs.implicitArguments, defns)
        ))
    )
    nres
  }
}

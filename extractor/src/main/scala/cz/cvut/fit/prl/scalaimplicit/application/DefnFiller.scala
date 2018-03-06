package cz.cvut.fit.prl.scalaimplicit.application

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ImplicitAnalysisResult,
  OrphanCallSites
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.{
  Argument,
  ArgumentLike,
  Declaration,
  ImplicitArgument
}

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

  def processArgList(args: Seq[ArgumentLike],
                     definitions: Set[Declaration]): Seq[ArgumentLike] = {
    args.map {
      case arg: Argument => arg
      case iarg: ImplicitArgument =>
        iarg.copy(
          declaration = findDeclOrReport(iarg, definitions),
          arguments = processArgList(iarg.arguments, definitions)
        )
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

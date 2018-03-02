package cz.cvut.fit.prl.scalaimplicit.core.extractor.representation

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractionResult
import Representation.{CallSite, Declaration, Location, Signature}

object SlimRepresentation {

  case class SlimCallSite(name: String,
                          code: String,
                          declaration: SlimDefinition,
                          location: Option[Location])
  object SlimCallSite {
    def apply(from: CallSite): SlimCallSite = new SlimCallSite(
      name = from.name,
      code = from.code,
      declaration = SlimDefinition(from.declaration),
      location = from.location
    )
  }

  case class SlimDefinition(
      name: String,
      kind: String,
      signature: String,
      paramLists: Seq[SlimParamList],
      location: Option[Location]
  ) {
    def kindedName: String = s"$kind $name"
  }
  object SlimDefinition {
    def apply(from: Declaration): SlimDefinition = new SlimDefinition(
      name = from.name,
      kind = from.kind,
      signature = from.signature.slim,
      paramLists = from.signature.get.parameterLists.map(pl =>
        SlimParamList(pl.params.size, pl.isImplicit)),
      location = from.location
    )
  }

  implicit class SlimSignature(that: Option[Signature]) {
    import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._
    def slim: String = PrettyInstances.PrettySignature.pretty(that, 0)
  }

  case class SlimParamList(paramNum: Int, isImplicit: Boolean)

  case class SlimResult(callSites: Seq[SlimCallSite],
                        definitions: Set[SlimDefinition])
  object SlimResult {
    def apply(res: ExtractionResult): SlimResult = new SlimResult(
      callSites = res.callSites.map(x => SlimCallSite(x)),
      definitions = res.declarations.map(x => SlimDefinition(x))
    )
  }
}

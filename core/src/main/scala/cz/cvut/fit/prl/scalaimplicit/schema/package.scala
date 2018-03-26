package cz.cvut.fit.prl.scalaimplicit

import com.trueaccord.scalapb.GeneratedMessageCompanion

package object schema {
  implicit val callSiteProtoCompanion: GeneratedMessageCompanion[CallSite] = CallSite
  implicit val declarationProtoCompanion: GeneratedMessageCompanion[Declaration] = Declaration
}

package cz.cvut.fit.prl.scalaimplicit.extractor

/**
  * Module to hold the internal representation of extracted information
  */
object Representation {
  case class Location(file: String, line: Int, col: Int)
  object Location {
    val Empty = Location("empty.scala", -1, -1)
  }

  trait TopLevelElem {
    def name: String
  }

  case class Type(name: String,
                  constraints: Option[String] = None,
                  parameters: Seq[Type] = Seq())

  case class Declaration(name: String,
                         kind: String,
                         location: Location,
                         isImplicit: Boolean,
                         signature: Option[Signature] = None,
                         parent: Option[Parent] = None) extends TopLevelElem
  case class Signature(typeParams: Seq[Type],
                       parameterLists: Seq[DeclaredParameterList],
                       returnType: Type)
  case class Parent(name: String,
                    declaration: Declaration,
                    typeArguments: Seq[Type])
  case class DeclaredParameterList(params: Seq[DeclaredParameter],
                                   isImplicit: Boolean)
  case class DeclaredParameter(name: String, tipe: Type)

  case class CallSite(name: String,
                      code: String,
                      location: Location,
                      isSynthetic: Boolean,
                      declaration: Declaration,
                      typeArguments: Seq[Type],
                      implicitArguments: Seq[ImplicitArgument]) extends TopLevelElem
  case class ImplicitArgument(name: String)
}

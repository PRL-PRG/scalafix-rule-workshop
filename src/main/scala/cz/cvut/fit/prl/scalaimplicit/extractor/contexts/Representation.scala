package cz.cvut.fit.prl.scalaimplicit.extractor.contexts

/**
  * Module to hold the internal representation of extracted information
  */
object Representation {

  trait TopLevelElem {
    def name: String
  }

  case class Location(file: String, line: Int, col: Int)
  case class Type(name: String,
                  constraints: Option[String] = None,
                  parameters: Seq[Type] = Seq())

  case class Declaration(name: String,
                         kind: String,
                         location: Option[Location],
                         isImplicit: Boolean,
                         signature: Option[Signature] = None,
                         parents: Seq[Parent] = Seq())
      extends TopLevelElem
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
                      location: Option[Location],
                      isSynthetic: Boolean,
                      declaration: Declaration,
                      typeArguments: Seq[Type],
                      implicitArguments: Seq[ImplicitArgument])
      extends TopLevelElem
  case class ImplicitArgument(name: String)
}

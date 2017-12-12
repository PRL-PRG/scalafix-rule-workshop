package cz.cvut.fit.prl.scalaimplicit.extractor.contexts

import org.langmeta.inputs.{Input, Position}
import sext._

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
                      implicitArguments: Seq[CallSite])
      extends TopLevelElem
  case class ImplicitArgument(name: String)
}

object Factories {
  import Representation._
  def createLocation(pos: Option[Position]): Option[Location] = {
    pos.map(p => {
      val file: String = p.input match {
        case Input.VirtualFile(path, _) => path
        case Input.File(path, _) => path.toString
        case _ => s"<unknown file: ${p.input}"
      }
      Location(file, p.endLine, p.endColumn)
    })
  }
  import scala.reflect.runtime.{universe => u}
  def createLocation(pos: u.Position): Option[Location] = {
    pos match {
      case p if p == u.NoPosition => None
      case p => Some(Location("<Reflective File>", p.line, p.column))
    }
  }
}

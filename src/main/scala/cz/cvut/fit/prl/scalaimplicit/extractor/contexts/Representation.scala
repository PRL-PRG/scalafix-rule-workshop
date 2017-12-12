package cz.cvut.fit.prl.scalaimplicit.extractor.contexts

import cz.cvut.fit.prl.scalaimplicit.extractor.Queries
import cz.cvut.fit.prl.scalaimplicit.extractor.Queries.{
  ReflectiveBreakdown,
  ReflectiveTArg
}
import org.langmeta.inputs.{Input, Position}
import sext._

/**
  * Module to hold the internal representation of extracted information
  */
object Representation {

  trait TopLevelElem {
    def name: String
  }

  case class Location(file: String, line: Int, col: Int) {
    override def toString: String = s"$file:$line:$col"
  }
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

  /**
    * Get the parent representation of a class symbol.
    * TODO We assume for now that we only want a single level of inheritance, but it's easy to make it recursive
    * @param reflection
    * @param ctx
    * @return
    */
  def createParent(reflection: u.Symbol, ctx: ReflectiveCtx): Parent = {
    Parent(
      name = reflection.fullName,
      declaration = Declaration(
        name = reflection.fullName,
        kind = ctx.getReflectiveKind(reflection.asClass),
        location = Factories.createLocation(reflection.pos),
        isImplicit = reflection.isImplicit,
        parents = Seq()
      ),
      typeArguments = Seq()
    )
  }

  def createSignature(ctx: ReflectiveCtx,
                      reflection: u.Symbol): Option[Signature] = {
    Some(
      Signature(
        typeParams = reflection.typeSignature.typeParams.map(t =>
          createTypeParameter(t.asType)),
        parameterLists = Seq(),
        returnType = Type("<Return Types Not Implemented Yet>")
      ))
  }

  def createDeclaration(ctx: ReflectiveCtx,
                        reflection: ReflectiveBreakdown): Declaration = {
    Declaration(
      name = reflection.originalSymbol.app.get.syntax,
      kind = ctx.getReflectiveKind(reflection.reflection),
      location = Factories.createLocation(reflection.reflection.pos),
      isImplicit = reflection.reflection.isImplicit,
      parents = reflection.reflection.typeSignature.baseClasses
        .map(createParent(_, ctx)),
      signature = createSignature(ctx, reflection.reflection)
    )
  }

  def createTypeConstraints(typeSignature: u.Type): Option[String] = {
    None
  }

  def createTypeParameter(tipe: u.TypeSymbol): Type = {
    Type(
      name = tipe.name.toString,
      constraints = createTypeConstraints(tipe.typeSignature),
      parameters = tipe.typeParams.map(t => createTypeParameter(t.asType))
    )
  }

  def createTypeArgument(targ: ReflectiveTArg): Type = {
    val symbol = targ.symbol
    Type(
      name = symbol.name.toString,
      constraints = createTypeConstraints(symbol.typeSignature),
      parameters = targ.args.map(createTypeArgument)
    )
  }

  def createCallSite(ctx: ReflectiveCtx,
                     reflection: Queries.ReflectiveBreakdown): CallSite = {

    val original = reflection.originalSymbol
    val reflect = reflection.reflection

    CallSite(
      location = Factories.createLocation(original.pos),
      name = original.app.get.syntax,
      code = "<No Code Yet>",
      isSynthetic = original.isSynthetic,
      declaration = createDeclaration(ctx, reflection),
      typeArguments = reflection.typeArguments.map(createTypeArgument),
      implicitArguments = reflection.params.map(createCallSite(ctx, _))
    )
  }
}

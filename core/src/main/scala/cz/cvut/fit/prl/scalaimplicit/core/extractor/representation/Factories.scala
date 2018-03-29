package cz.cvut.fit.prl.scalaimplicit.core.extractor.representation

import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx
import org.langmeta.inputs.{Input, Position}

import scala.reflect.runtime.{universe => u}

object Factories {
  import cz.cvut.fit.prl.scalaimplicit.schema._

  def createLocation(pos: Position = Position.None): Option[Location] = {
    pos match {
      case p: Position if p == Position.None => None
      case p => {
        val file: String = pos.input match {
          case Input.VirtualFile(path, _) => path
          case Input.File(path, _) => path.toString
          case _ => s"<unknown file: ${pos.input}"
        }
        Some(Location(file, pos.endLine, pos.endColumn))
      }
    }
  }

  /**
    * Get the parent representation of a class symbol.
    * TODO We assume for now that we only want a single level of inheritance, but it's easy to make it recursive
    * @param parent
    * @param ctx
    * @return
    */
  def createParent(child: DeclarationReflection,
                   parent: ParentReflection,
                   ctx: ReflectiveCtx): Parent = {
    Parent(
      name = parent.fullName,
      declaration = Declaration(
        name = parent.declaration.fullName,
        kind = parent.declaration.kind,
        location = createLocation(parent.declaration.position),
        isImplicit = parent.declaration.isImplicit,
        signature = createSignature(ctx, parent.declaration),
        parents = Seq()
      ),
      typeArguments = parent.typeArguments.map(createTypeArgument)
    )
  }

  def createParam(ctx: ReflectiveCtx, symbol: u.Symbol): Parameter = {
    Parameter(
      name = symbol.name.toString,
      parameterType = createTypeParameter(symbol.typeSignature.typeSymbol.asType)
    )
  }

  def createParamList(ctx: ReflectiveCtx,
                      paramList: List[u.Symbol]): ParameterList = {
    ParameterList(
      isImplicit = paramList.nonEmpty && paramList.head.isImplicit, // We assume that if one param is implicit, every param is
      parameters = paramList.map(createParam(ctx, _))
    )
  }

  def createSignature(ctx: ReflectiveCtx,
                      reflection: DeclarationReflection): Option[Signature] = {

    val typeParams = reflection.typeSignature.typeParams.map(t =>
      createTypeParameter(t.asType))

    Some(
      Signature(
        typeParameters = typeParams,
        parameterLists = reflection.paramLists.map(createParamList(ctx, _)),
        returnType = createTypeArgument(reflection.returnType)
      ))
  }

  def createDeclaration(ctx: ReflectiveCtx,
                        reflection: DeclarationReflection): Declaration = {
    Declaration(
      name = reflection.fullName,
      kind = reflection.kind,
      location = createLocation(reflection.position),
      isImplicit = reflection.isImplicit,
      parents = reflection.baseClasses
        .map(createParent(reflection, _, ctx)),
      signature = createSignature(ctx, reflection)
    )
  }

  def createTypeParameter(tipe: u.TypeSymbol): Type = {
    Type(
      name = tipe.fullName,
      parameters = tipe.typeParams.map(t => createTypeParameter(t.asType))
    )
  }

  def createTypeArgument(t: u.Type): Type = {
    Type(t.toString, t.typeArgs.map(createTypeArgument))
  }

  def createTypeArgument(targ: ReflectiveTArg): Type = {
    Type(
      name = targ.fullName,
      parameters = targ.args.map(createTypeArgument)
    )
  }

  def createImplicitArgument(ctx: ReflectiveCtx, param: Param): Argument = {
    param match {
      case reflection: ImplicitReflection => {
        val original = reflection.originalSymbol
        Argument(
          code = reflection.code,
          info = Some(ArgumentInfo(
            name = reflection.fullName,
            declaration = createDeclaration(ctx, reflection.declaration),
            typeArguments = reflection.typeArguments.map(createTypeArgument),
            arguments = reflection.args.map(createImplicitArgument(ctx, _))
          ))
        )
      }
      case p: Param => {
        Argument(p.code)
      }
    }

  }

  def createCallSite(ctx: ReflectiveCtx,
                     reflection: ImplicitReflection): CallSite = {

    val original = reflection.originalSymbol

    CallSite(
      location = createLocation(reflection.pos),
      name = reflection.fullName,
      code = reflection.code,
      isSynthetic = original.isSynthetic,
      declaration = createDeclaration(ctx, reflection.declaration),
      typeArguments = reflection.typeArguments.map(createTypeArgument),
      implicitArguments = reflection.args.map(createImplicitArgument(ctx, _))
    )
  }
}

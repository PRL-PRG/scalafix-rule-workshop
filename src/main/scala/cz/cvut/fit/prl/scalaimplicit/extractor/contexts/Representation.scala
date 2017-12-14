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
  case class Type(name: String, parameters: Seq[Type] = Seq())

  case class Declaration(name: String,
                         kind: String,
                         location: Option[Location],
                         isImplicit: Boolean,
                         signature: Option[Signature] = None,
                         parents: Seq[Parent] = Seq())
      extends TopLevelElem
  case class Signature(typeParams: Seq[Type] = Seq(),
                       parameterLists: Seq[DeclaredParameterList] = Seq(),
                       returnType: Option[Type] = None)
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
    /* pos.map(p => {
      val file: String = p.input match {
        case Input.VirtualFile(path, _) => path
        case Input.File(path, _) => path.toString
        case _ => s"<unknown file: ${p.input}"
      }
      Location(file, p.endLine, p.endColumn)
    })*/
    // TODO Uncomment the code above when we have normalized test results
    None
  }
  import scala.reflect.runtime.{universe => u}
  def createLocation(pos: u.Position): Option[Location] = {
    /*pos match {
      case p if p == u.NoPosition => None
      case p => Some(Location("<Reflective File>", p.line, p.column))
    }*/
    // TODO Uncomment the code above when we have normalized test results
    None
  }

  /**
    * Get the parent representation of a class symbol.
    * TODO We assume for now that we only want a single level of inheritance, but it's easy to make it recursive
    * @param parent
    * @param ctx
    * @return
    */
  def createParent(child: u.Symbol,
                   parent: u.Symbol,
                   ctx: ReflectiveCtx): Parent = {
    val parentType = child.typeSignature.baseType(parent)
    Parent(
      name = parent.fullName,
      declaration = Declaration(
        name = parent.fullName,
        kind = ctx.getReflectiveKind(parent.asClass),
        location = Factories.createLocation(parent.pos),
        isImplicit = parent.isImplicit,
        signature = createSignature(ctx, parent),
        parents = Seq()
      ),
      typeArguments = parentType.typeArgs.map(createTypeArgument)
    )
  }

  def createParam(ctx: ReflectiveCtx, symbol: u.Symbol): DeclaredParameter = {
    DeclaredParameter(
      name = symbol.name.toString,
      tipe = createTypeParameter(symbol.typeSignature.typeSymbol.asType)
    )
  }

  def createParamList(ctx: ReflectiveCtx,
                      paramList: List[u.Symbol]): DeclaredParameterList = {
    DeclaredParameterList(
      isImplicit = paramList.head.isImplicit, // We assume that if one param is implicit, every param is
      params = paramList.map(createParam(ctx, _))
    )
  }

  def createSignature(ctx: ReflectiveCtx,
                      reflection: u.Symbol): Option[Signature] = {
    def createReturnType(tipe: u.Type): Option[Type] = {
      Some(
        Type(
          name = tipe.toString,
          parameters = tipe.typeArgs.map(t => createReturnType(t).get)
        ))
    }

    val typeParams = reflection.typeSignature.typeParams.map(t =>
      createTypeParameter(t.asType))

    val params = reflection match {
      case r if r.isMethod => r.asMethod.paramLists
      case _ =>
        println(s"Not a method ${reflection.toString}"); List()
    }
    reflection match {
      case refl if refl.isMethod =>
        Some(
          Signature(
            typeParams = typeParams,
            parameterLists =
              refl.asMethod.paramLists.map(createParamList(ctx, _)),
            returnType = createReturnType(refl.asMethod.returnType)
          ))
      case refl =>
        Some(
          Signature(typeParams = typeParams,
                    parameterLists = Seq(),
                    returnType = None))
    }
  }

  def createDeclaration(ctx: ReflectiveCtx,
                        reflection: ReflectiveBreakdown): Declaration = {
    def firstLevelBaseClasses(bases: List[u.Symbol]) = {
      // Take the tail because the first one is the self definition
      // Remove the classes that are parents of some class in bases
      bases match {
        case bases if bases.isEmpty => Seq()
        case bases =>
          bases.tail.filterNot(cls =>
            bases.tail.exists(_.typeSignature.baseClasses.tail.contains(cls)))
      }
    }
    val symbol = reflection.reflection

    Declaration(
      name = symbol.fullName,
      kind = ctx.getReflectiveKind(symbol),
      location = Factories.createLocation(symbol.pos),
      isImplicit = symbol.isImplicit,
      parents = firstLevelBaseClasses(symbol.typeSignature.baseClasses)
        .map(createParent(symbol, _, ctx)),
      signature = createSignature(ctx, symbol)
    )
  }

  def createTypeParameter(tipe: u.TypeSymbol): Type = {
    Type(
      name = tipe.fullName,
      parameters = tipe.typeParams.map(t => createTypeParameter(t.asType))
    )
  }

  def createTypeArgument(symbol: u.Type): Type = {
    Type(symbol.toString, symbol.typeArgs.map(createTypeArgument))
  }

  def createTypeArgument(targ: ReflectiveTArg): Type = {
    val symbol = targ.symbol
    Type(
      name = symbol.fullName,
      parameters = targ.args.map(createTypeArgument)
    )
  }

  def createCallSite(ctx: ReflectiveCtx,
                     reflection: Queries.ReflectiveBreakdown): CallSite = {

    val original = reflection.originalSymbol
    val reflect = reflection.reflection

    CallSite(
      location = Factories.createLocation(original.pos),
      name = reflect.fullName,
      code = "<No Code Yet>",
      isSynthetic = original.isSynthetic,
      declaration = createDeclaration(ctx, reflection),
      typeArguments = reflection.typeArguments.map(createTypeArgument),
      implicitArguments = reflection.params.map(createCallSite(ctx, _))
    )
  }
}

object PrettyPrinters {
  import Representation._
  trait PrettyPrintable[T] {
    def pretty(t: T, indent: Int): String
  }

  object PrettyInstances {

    implicit object PrettyType extends PrettyPrintable[Type] {
      override def pretty(t: Type, indent: Int): String = {
        prettyPrint(t.parameters, indent + 2) match {
          case p if p == "" => s"${t.name}"
          case p => s"${t.name}[$p]"
        }
      }
    }

    implicit object PrettyDeclaredParameter
        extends PrettyPrintable[DeclaredParameter] {
      override def pretty(t: DeclaredParameter, indent: Int): String = {
        s"${t.name}: ${prettyPrint(t.tipe, indent + 2)}"
      }
    }

    implicit object PrettyDeclaredParameterList
        extends PrettyPrintable[DeclaredParameterList] {
      override def pretty(t: DeclaredParameterList, indent: Int): String = {
        val prefix = if (t.isImplicit) s"implicit " else ""
        s"(${prefix}${prettyPrint(t.params, indent + 2)})"
      }
    }

    implicit object PrettySignature
        extends PrettyPrintable[Option[Signature]] {
      override def pretty(sign: Option[Signature], indent: Int): String = {
        sign match {
          case Some(sign) =>
            s"${" " * indent}Signature: [${prettyPrint(sign.typeParams)}]${prettyPrint(
              sign.parameterLists)}: ${prettyPrint(
              sign.returnType.getOrElse(Type("")))}"
          case None => ""
        }

      }
    }

    implicit object PrettyParent extends PrettyPrintable[Parent] {
      override def pretty(t: Parent, indent: Int): String = {
        s"\n${" " * indent}Parent: ${t.name}[${prettyPrint(t.typeArguments, indent + 2)}]" +
          s"${nlTrim(prettyPrint(t.declaration, indent + 2)(PrettyDeclaration))}"
      }
    }

    implicit object PrettyDeclaration extends PrettyPrintable[Declaration] {
      override def pretty(t: Declaration, indent: Int): String = {
        val realKind = if (t.isImplicit) s"implicit ${t.kind}" else t.kind
        s"${" " * indent}Declaration: ${t.name}, $realKind, ${t.location}" +
          s"${nlTrim(prettyPrint(t.signature, indent + 2))}" +
          s"${nlTrim(prettyPrint(t.parents, indent + 2))}"
      }
    }

    implicit object PrettyCallSite extends PrettyPrintable[CallSite] {
      def pretty(cs: CallSite, indent: Int): String = {
        val prefix = if (cs.isSynthetic) "synthetic " else ""
        s"""${" " * indent}${prefix}CallSite: ${cs.name}, ${cs.code}, ${cs.location}
           |${" " * indent}${prettyPrint(cs.declaration, indent + 2)}
           |${" " * indent}${nlTrim(prettyPrint(cs.typeArguments, indent + 2))}
           |${" " * indent}${nlTrim(
             prettyPrint(cs.implicitArguments, indent + 2))}""".stripMargin
      }
    }

    implicit object PrettyTopLevel extends PrettyPrintable[TopLevelElem] {
      override def pretty(t: TopLevelElem, indent: Int): String = {
        t match {
          case t: CallSite => prettyPrint(t, indent)
          case t: Declaration => prettyPrint(t, indent)
        }
      }
    }

    implicit def PrettySeq[T: PrettyPrintable]: PrettyPrintable[Seq[T]] =
      (seq: Seq[T], indent: Int) =>
        if (seq.nonEmpty)
          seq.map(elem => s"${prettyPrint(elem, indent)}").mkString(",")
        else ""

    def nlTrim(str: String) = if (str.nonEmpty) s"\n$str" else ""
  }

  def prettyPrint[T](some: T, startIndent: Int = 0)(
      implicit printer: PrettyPrintable[T]): String = {
    val res = printer.pretty(some, startIndent)
    res
  }
}

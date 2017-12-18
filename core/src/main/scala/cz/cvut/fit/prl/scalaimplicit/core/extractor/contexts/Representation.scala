package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

import boopickle.DefaultBasic
import boopickle.DefaultBasic.PicklerGenerator
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ExtractionResult, Queries}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.Queries.{
  ReflectiveBreakdown,
  ReflectiveTArg
}
import org.langmeta.inputs.{Input, Position}

/**
  * Module to hold the internal representation of extracted information
  */
object Representation {

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
                      implicitArguments: Seq[ImplicitArgument])
  case class ImplicitArgument(name: String,
                              code: String,
                              location: Option[Location],
                              declaration: Declaration,
                              typeArguments: Seq[Type],
                              arguments: Seq[ImplicitArgument])
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

    val typeParams = reflection.typeSignature.typeParams.map(t =>
      createTypeParameter(t.asType))

    val params = reflection match {
      case r if r.isMethod => r.asMethod.paramLists
      case _ => List()
    }
    reflection match {
      case refl if refl.isMethod =>
        Some(
          Signature(
            typeParams = typeParams,
            parameterLists =
              refl.asMethod.paramLists.map(createParamList(ctx, _)),
            returnType = Some(createTypeArgument(refl.asMethod.returnType))
          ))
      case refl =>
        Some(
          Signature(typeParams = typeParams,
                    parameterLists = Seq(),
                    returnType = Some(createTypeArgument(refl.typeSignature))))
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

  def createTypeArgument(t: u.Type): Type = {
    Type(t.toString, t.typeArgs.map(createTypeArgument))
  }

  def createTypeArgument(targ: ReflectiveTArg): Type = {
    val symbol = targ.symbol
    Type(
      name = symbol.fullName,
      parameters = targ.args.map(createTypeArgument)
    )
  }

  def createImplicitArgument(
      ctx: ReflectiveCtx,
      reflection: Queries.ReflectiveBreakdown): ImplicitArgument = {

    val original = reflection.originalSymbol
    val reflect = reflection.reflection
    ImplicitArgument(
      location = Factories.createLocation(original.pos),
      name = reflect.fullName,
      code = "<No Code Yet>",
      declaration = createDeclaration(ctx, reflection),
      typeArguments = reflection.typeArguments.map(createTypeArgument),
      arguments = reflection.params.map(createImplicitArgument(ctx, _))
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
      implicitArguments = reflection.params.map(createImplicitArgument(ctx, _))
    )
  }
}

object Gatherer {
  import Representation._
  private def gatherDeclarations(
      implicitArgument: ImplicitArgument): Set[Declaration] = {
    Set(implicitArgument.declaration) ++ implicitArgument.arguments.flatMap(
      gatherDeclarations)
  }

  def gatherDeclarations(cs: CallSite): Set[Declaration] = {
    Set(cs.declaration) ++ cs.implicitArguments
      .flatMap(gatherDeclarations)
      .toSet
  }

  def gatherDeclarations(cs: Seq[CallSite]): Set[Declaration] = {
    cs.flatMap(gatherDeclarations).toSet
  }
}

object PrettyPrinters {
  import Representation._
  trait PrettyPrintable[T] {
    def pretty(t: T, indent: Int): String
  }

  object PrettyInstances {

    implicit object PrettyLocation extends PrettyPrintable[Option[Location]] {
      override def pretty(t: Option[Location], indent: Int): String = {
        t match {
          case Some(loc) => s"[${loc.toString}]:"
          case None => "?:"
        }
      }
    }

    implicit object PrettyType extends PrettyPrintable[Type] {
      override def pretty(t: Type, indent: Int): String = {
        prettyPrint(t.parameters, indent + 2) match {
          case p if p == "" => s"${t.name}"
          case p => s"${t.name}[$p]"
        }
      }
    }

    implicit object PrettyTypeOption extends PrettyPrintable[Option[Type]] {
      override def pretty(t: Option[Type], indent: Int): String = {
        t match {
          case Some(tipe) => prettyPrint(tipe)
          case None => ""
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
          case Some(sign) => {
            val typeParams = wrapIfSome(prettyPrint(sign.typeParams), "[", "]")
            val parameterLists = prettyPrint(sign.parameterLists)
            val retType = wrapIfSome(prettyPrint(sign.returnType), ": ")
            s"$typeParams$parameterLists$retType"
          }
          case None => ""
        }
      }
    }

    implicit object PrettyParent extends PrettyPrintable[Parent] {
      def getMatchedTypes(tparams: Seq[Type], targs: Seq[Type]): String = {
        assert(tparams.size == targs.size,
               "Different number of type parameters and arguments")
        (tparams zip targs)
          .map(pair => s"${prettyPrint(pair._1)} = ${prettyPrint(pair._2)}")
          .mkString(",")
      }

      override def pretty(t: Parent, indent: Int): String = {
        val tparams = t.declaration.signature.get.typeParams
        val targs =
          wrapIfSome(getMatchedTypes(tparams, t.typeArguments), "[", "]")
        val kind =
          if (t.declaration.isImplicit) s"implicit ${t.declaration.kind}"
          else t.declaration.kind
        s"$kind ${t.name}$targs"
      }
    }

    implicit object PrettyDeclaration extends PrettyPrintable[Declaration] {
      override def pretty(t: Declaration, indent: Int): String = {
        val realKind = if (t.isImplicit) s"implicit ${t.kind}" else t.kind
        val signature = prettyPrint(t.signature, indent + 2)
        val parents =
          wrapIfSome(prettyPrint(t.parents, indent + 2), " extends (", ")")
        s"${prettyPrint(t.location)}${" " * indent}$realKind ${t.name}"
          .concat(signature)
          .concat(parents)
      }
    }

    implicit object PrettyArgument extends PrettyPrintable[ImplicitArgument] {
      override def pretty(t: ImplicitArgument, indent: Int): String = {
        s"""${prettyPrint(t.location)}${" " * indent}iarg: ${t.name}${wrapIfSome(
             prettyPrint(t.typeArguments, indent + 2),
             "[",
             "]")}
          |${prettyPrint(t.declaration, indent + 2)}
          |${prettyPrint(t.arguments, indent + 2)}""".stripMargin
      }
    }

    implicit object PrettyCallSite extends PrettyPrintable[CallSite] {
      def pretty(cs: CallSite, indent: Int): String = {
        val prefix = if (cs.isSynthetic) "s" else ""
        s"""${prettyPrint(cs.location)}${" " * indent}${prefix}cs: ${cs.name}${wrapIfSome(
             prettyPrint(cs.typeArguments, indent + 2),
             "[",
             "]")}
           |${prettyPrint(cs.declaration, indent + 2)}
           |${prettyPrint(cs.implicitArguments, indent + 2)}""".stripMargin
      }
    }

    implicit val separator: String = ", "
    implicit def PrettySeq[T](
        implicit separator: String,
        printer: PrettyPrintable[T]): PrettyPrintable[Seq[T]] =
      (seq: Seq[T], indent: Int) =>
        if (seq.nonEmpty)
          seq
            .map(elem => s"${printer.pretty(elem, indent)}")
            .mkString(separator)
        else ""

    def wrapIfSome(content: String, begin: String = "", end: String = "") =
      if (content.nonEmpty) begin + content + end
      else ""
  }

  def prettyPrint[T](some: T, startIndent: Int = 0)(
      implicit printer: PrettyPrintable[T]): String = {
    val res = printer.pretty(some, startIndent)
    res
  }
}

object Serializer {
  object Picklers {
    import Representation._
    import boopickle.Default._
    implicit val typepickler = PicklerGenerator.generatePickler[Type]
    implicit val signpickler = PicklerGenerator.generatePickler[Signature]
    implicit val iargpickler =
      PicklerGenerator.generatePickler[ImplicitArgument]
    implicit val locpickler = PicklerGenerator.generatePickler[Location]
    implicit val declpickler = PicklerGenerator.generatePickler[Declaration]
    implicit val cspickler = PicklerGenerator.generatePickler[CallSite]
    implicit val respickler =
      PicklerGenerator.generatePickler[ExtractionResult]
  }

  import boopickle.DefaultBasic._
  import Picklers._
  def save(res: ExtractionResult, file: String) =
    Files.write(Paths.get(file), Pickle.intoBytes(res).array())

  def load(file: String): ExtractionResult =
    Unpickle[ExtractionResult].fromBytes(
      ByteBuffer.wrap(Files.readAllBytes(Paths.get(file))))
}

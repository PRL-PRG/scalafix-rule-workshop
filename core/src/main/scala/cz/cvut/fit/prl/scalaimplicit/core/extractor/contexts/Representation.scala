package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}

import boopickle.DefaultBasic
import boopickle.DefaultBasic.PicklerGenerator
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ExtractionResult, Queries}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts.{
  Param,
  Reflection,
  ReflectiveTArg
}
import org.langmeta.inputs.{Input, Position}
import org.langmeta.semanticdb.Denotation

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
                      implicitArguments: Seq[ArgumentLike])
  trait ArgumentLike {
    def code: String
    def location: Option[Location]
  }
  case class Argument(code: String, location: Option[Location])
      extends ArgumentLike
  case class ImplicitArgument(name: String,
                              code: String,
                              location: Option[Location],
                              declaration: Declaration,
                              typeArguments: Seq[Type],
                              arguments: Seq[ArgumentLike])
      extends ArgumentLike
}

object Factories {
  import Representation._

  def createLocation(pos: Position): Option[Location] = {
    val file: String = pos.input match {
      case Input.VirtualFile(path, _) => path
      case Input.File(path, _) => path.toString
      case _ => s"<unknown file: ${pos.input}"
    }
    Some(Location(file, pos.endLine, pos.endColumn))
  }

  def createLocation(reflection: Reflection): Option[Location] = {
    if (reflection.hasLocation) createLocation(reflection.pos)
    else None
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
  def createParent(child: Reflection,
                   parent: Reflection,
                   ctx: ReflectiveCtx): Parent = {
    Parent(
      name = parent.fullName,
      declaration = Declaration(
        name = parent.fullName,
        kind = parent.kind,
        location = Factories.createLocation(parent),
        isImplicit = parent.isImplicit,
        signature = createSignature(ctx, parent),
        parents = Seq()
      ),
      typeArguments = parent.typeArguments.map(createTypeArgument)
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
                      reflection: Reflection): Option[Signature] = {

    val typeParams = reflection.typeSignature.typeParams.map(t =>
      createTypeParameter(t.asType))

    Some(
      Signature(
        typeParams = typeParams,
        parameterLists = reflection.paramLists.map(createParamList(ctx, _)),
        returnType = Some(createTypeArgument(reflection.returnType))
      ))
  }

  def createDeclaration(ctx: ReflectiveCtx,
                        reflection: Reflection): Declaration = {
    Declaration(
      name = reflection.fullName,
      kind = reflection.kind,
      location = Factories.createLocation(reflection),
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
      name = targ.reflection.fullName,
      parameters = targ.args.map(createTypeArgument)
    )
  }

  def createImplicitArgument(ctx: ReflectiveCtx, param: Param): ArgumentLike = {
    param match {
      case reflection: Reflection => {
        val original = reflection.originalSymbol
        ImplicitArgument(
          location = Factories.createLocation(reflection.pos),
          name = reflection.fullName,
          code = reflection.code,
          declaration = createDeclaration(ctx, reflection),
          typeArguments = reflection.typeArguments.map(createTypeArgument),
          arguments = reflection.params.map(createImplicitArgument(ctx, _))
        )
      }
      case p: Param => {
        Argument(p.code, Factories.createLocation(p.pos))
      }
    }

  }

  def createCallSite(ctx: ReflectiveCtx, reflection: Reflection): CallSite = {

    val original = reflection.originalSymbol

    CallSite(
      location = Factories.createLocation(reflection),
      name = reflection.fullName,
      code = reflection.code,
      isSynthetic = original.isSynthetic,
      declaration = createDeclaration(ctx, reflection),
      typeArguments = reflection.typeArguments.map(createTypeArgument),
      implicitArguments = reflection.params.map(createImplicitArgument(ctx, _))
    )
  }
}

object Gatherer {
  import Representation._
  //TODO: We should actually gather declarations from the tree instead of the implicits

  /*
  private def gatherDeclarations(
      implicitArgument: ImplicitArgument): Set[Declaration] = {
    //TODO: We should actually gather declarations from the tree instead of the implicits
    Set(implicitArgument.declaration) ++ implicitArgument.arguments.collect {
      case a: ImplicitArgument => gatherDeclarations(a)
    }
  }

  def gatherDeclarations(cs: CallSite): Set[Declaration] = {
    //TODO: We should actually gather declarations from the tree instead of the implicits
    Set(cs.declaration) ++ cs.implicitArguments.collect {
      case a: ImplicitArgument => gatherDeclarations(a)
    }.toSet
  }
   */

  //TODO: We should actually gather declarations from the tree instead of the implicits
  def gatherDeclarations(cs: Seq[CallSite]): Set[Declaration] = Set()

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

    implicit object PrettyArgument extends PrettyPrintable[ArgumentLike] {
      override def pretty(arg: ArgumentLike, indent: Int): String = {
        arg match {
          case t: Argument => {
            s"""${prettyPrint(t.location)}${" " * indent}arg: ${t.code}"""
          }
          case t: ImplicitArgument => {
            s"""${prettyPrint(t.location)}${" " * indent}iarg: ${t.name}${wrapIfSome(
                 prettyPrint(t.typeArguments, indent + 2),
                 "[",
                 "]")}
               |${prettyPrint(t.declaration, indent + 2)}
               |${prettyPrint(t.arguments, indent + 2)}""".stripMargin
          }
        }
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
/*
object Serializer {
  object Picklers {
    import Representation._
    import boopickle.Default._
    implicit val typepickler = PicklerGenerator.generatePickler[Type]
    implicit val signpickler = PicklerGenerator.generatePickler[Signature]
    implicit val iargpickler =
      PicklerGenerator.generatePickler[ImplicitArgument]
    implicit val argpicler = PicklerGenerator.generatePickler[Argument]
    implicit val locpickler = PicklerGenerator.generatePickler[Location]
    implicit val declpickler = PicklerGenerator.generatePickler[Declaration]
    implicit val cspickler = PicklerGenerator.generatePickler[CallSite]
    implicit val respickler =
      PicklerGenerator.generatePickler[ExtractionResult]
  }

  import boopickle.DefaultBasic._
  import Picklers._
  def save(res: ExtractionResult, file: String) =
    Files.write(Paths.get(file), Pickle.intoBytes(res.callSites).array())

  def load(file: String): ExtractionResult = {
    val css = Unpickle[Seq[CallSite]]
      .fromBytes(ByteBuffer.wrap(Files.readAllBytes(Paths.get(file))))
    ExtractionResult(css, Set())
  }
}
 */
object JSONSerializer {
  import org.json4s._
  import org.json4s.native.JsonMethods._
  object DSL {
    import Representation._
    import org.json4s.JsonDSL._
    trait Jsonable[T] {
      def json(v: T): JValue
      //def unjson(v: JValue): T
    }

    implicit object JLocation extends Jsonable[Location] {
      def json(v: Location) =
        "location" ->
          ("line" -> v.line) ~
            ("col" -> v.col) ~
            ("file" -> v.file)
    }

    implicit object JLocationOption extends Jsonable[Option[Location]] {
      def json(v: Option[Location]) =
        v match {
          case Some(loc) => JLocation.json(loc)
          case None => JNothing
        }
    }

    implicit object JTypeOption extends Jsonable[Option[Type]] {
      def json(v: Option[Type]) =
        v match {
          case Some(loc) => JType.json(loc)
          case None => JNothing
        }
    }

    implicit object JTypeSeq extends Jsonable[Seq[Type]] {
      def json(v: Seq[Type]) =
        v.map(JType.json)
    }
    implicit object JType extends Jsonable[Type] {
      def json(v: Type) =
        "type" ->
          ("name" -> v.name) ~
            ("tparams" -> JTypeSeq.json(v.parameters))
    }

    implicit object JDParamSeq extends Jsonable[Seq[DeclaredParameter]] {
      def json(v: Seq[DeclaredParameter]) =
        v.map(JDeclaredParameter.json)
    }

    implicit object JDeclaredParameter extends Jsonable[DeclaredParameter] {
      override def json(v: DeclaredParameter): JValue =
        "declared_param" -> ("name" -> v.name) ~ ("typee" -> JType.json(
          v.tipe))
    }

    implicit object JDParamListSeq
        extends Jsonable[Seq[DeclaredParameterList]] {
      def json(v: Seq[DeclaredParameterList]) =
        v.map(JDeclaredParameterList.json)
    }

    implicit object JDeclaredParameterList
        extends Jsonable[DeclaredParameterList] {
      def json(v: DeclaredParameterList) =
        "declared_param_list" ->
          ("is_implicit" -> JBool(v.isImplicit)) ~
            ("params" -> JDParamSeq.json(v.params))
    }

    implicit object JSignatureOption extends Jsonable[Option[Signature]] {
      def json(v: Option[Signature]) =
        v match {
          case Some(loc) => JSignature.json(loc)
          case None => JNothing
        }
    }

    implicit object JSignature extends Jsonable[Signature] {
      def json(v: Signature): JValue =
        "signature" ->
          ("tparams" -> JTypeSeq.json(v.typeParams)) ~
            ("params" -> JDParamListSeq.json(v.parameterLists)) ~
            ("rettype" -> JTypeOption.json(v.returnType))
    }

    implicit object JParentSeq extends Jsonable[Seq[Parent]] {
      def json(v: Seq[Parent]) =
        v.map(JParent.json)
    }

    implicit object JParent extends Jsonable[Parent] {
      def json(v: Parent) =
        "parent" ->
          ("name" -> JString(v.name)) ~
            ("declaration" -> JDeclaration.json(v.declaration)) ~
            ("targs" -> JTypeSeq.json(v.typeArguments))
    }

    implicit object JDeclarationSeq extends Jsonable[Seq[Declaration]] {
      def json(v: Seq[Declaration]) =
        v.map(JDeclaration.json)
    }

    implicit object JDeclaration extends Jsonable[Declaration] {
      def json(v: Declaration): JValue =
        "declaration" ->
          ("name" -> v.name) ~
            ("kind" -> v.kind) ~
            ("location" -> JLocationOption.json(v.location)) ~
            ("isImplicit" -> v.isImplicit) ~
            ("signature" -> JSignatureOption.json(v.signature)) ~
            ("parents" -> JParentSeq.json(v.parents))
    }

    implicit object JArgSeq extends Jsonable[Seq[ArgumentLike]] {
      def json(v: Seq[ArgumentLike]): JValue =
        v.collect {
          case a: ImplicitArgument => JImplicitArgument.json(a)
          case a: Argument => JArgument.json(a)
        }
    }

    implicit object JArgument extends Jsonable[Argument] {
      def json(v: Argument): JValue =
        "argument" ->
          ("code" -> JString(v.code)) ~
            ("location" -> JLocationOption.json(v.location))
    }

    implicit object JImplicitArgument extends Jsonable[ImplicitArgument] {
      def json(v: ImplicitArgument) =
        "argument" ->
          ("name" -> v.name) ~
            ("code" -> v.code) ~
            ("location" -> JLocationOption.json(v.location)) ~
            ("declaration" -> JDeclaration.json(v.declaration)) ~
            ("targs" -> JTypeSeq.json(v.typeArguments)) ~
            ("impl_args" -> JArgSeq.json(v.arguments))
    }

    implicit object JCallSiteSeq extends Jsonable[Seq[CallSite]] {
      def json(v: Seq[CallSite]) =
        v.map(JCallSite.json)
    }

    implicit object JCallSite extends Jsonable[CallSite] {
      def json(v: CallSite) =
        "callsite" ->
          ("name" -> v.name) ~
            ("code" -> v.code) ~
            ("location" -> JLocationOption.json(v.location)) ~
            ("isSynthetic" -> v.isSynthetic) ~
            ("declaration" -> JDeclaration.json(v.declaration)) ~
            ("targs" -> JTypeSeq.json(v.typeArguments)) ~
            ("impl_args" -> JArgSeq.json(v.implicitArguments))
    }

    implicit object JRes extends Jsonable[ExtractionResult] {
      def json(v: ExtractionResult) =
        "extraction_result" ->
          ("callSites" -> JCallSiteSeq.json(v.callSites)) ~
            ("declarations" -> JDeclarationSeq.json(v.declarations.toSeq))
    }
  }

  import DSL._
  def save(res: ExtractionResult, file: String) =
    Files.write(Paths.get(file),
                pretty(render(JRes.json(res))).toString.getBytes)

  def load(file: String): ExtractionResult = {
    implicit val formats = org.json4s.DefaultFormats
    val json = parse(io.Source.fromFile(file).mkString)
      .extract[ExtractionResult]

    ExtractionResult(Seq(), Set())
  }
}

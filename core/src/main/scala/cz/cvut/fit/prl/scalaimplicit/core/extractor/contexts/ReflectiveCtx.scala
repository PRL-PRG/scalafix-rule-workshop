package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ErrorCollection, ImplicitAnalysisResult}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.decomposers.PatDecomposer
import cz.cvut.fit.prl.scalaimplicit.{schema => s}
import org.langmeta.inputs.{Input, Position}
import org.langmeta.semanticdb.{Database, Denotation}
import org.langmeta.semanticdb.Signature._

import scala.meta.internal.SemanticdbPlugin
import scala.meta.internal.semanticdb.DatabaseOps
import scala.meta.{Database, Self, Signature, Symbol, Synthetic, Term, Tree, Type, _}
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.io.VirtualDirectory
import scala.tools.nsc.reporters.StoreReporter
import scala.util.{Failure, Success, Try}

class ReflectiveCtx(compiler: Global, db: Database) extends SemanticCtx(db) {
  val ops: DatabaseOps = new DatabaseOps {
    val global: compiler.type = compiler
  }

  import ops._

  // Compiler -> Scalameta
  private def toMeta(symbol: g.Symbol): Symbol = symbol.toSemantic

  // Scalameta -> Compiler
  def toGlobal(symbol: Symbol): Try[g.Symbol] = {
    def loop(symbol: Symbol): g.Symbol = symbol match {
      case Symbol.None =>
        g.NoSymbol
      case Symbol.Global(Symbol.None, Signature.Term("_root_")) =>
        g.rootMirror.RootClass
      case Symbol.Global(Symbol.None, Signature.Term("_empty_")) =>
        g.rootMirror.EmptyPackage
      case Symbol.Global(qual, Signature.Term(name)) =>
        val owner = loop(qual)
        val n = g.TermName(name)
        owner.info.member(n) match {
          case g.NoSymbol =>
            val candidates = owner.info.members.filter(_.nameString == name)
            if (candidates.size == 0) {
              g.NoSymbol
            } else if (candidates.size == 1) {
              candidates.head
            } else {
              owner.newOverloaded(g.NoPrefix, candidates.toList)
            }
          case x => x
        }
      case Symbol.Global(qual, Signature.Type(name)) =>
        val owner = loop(qual)
        if (owner.isMethod) {
          owner.info.typeParams.find(_.nameString == name).get
        } else {
          owner.info.member(g.TypeName(name))
        }
      case Symbol.Global(qual, Signature.TypeParameter(name)) =>
        val owner = loop(qual)
        owner.typeParams.find(_.nameString == name).get
      case Symbol.Global(qual, Signature.TermParameter(name)) =>
        val owner = loop(qual)
        // this goes below, in termparameter
        if (owner.isMethod) {
          owner.info.params.find(_.nameString == name).get
        } else if (owner.isClass || owner.isModule) {
          owner.info.member(g.TermName(name))
        } else {
          throw new RuntimeException("Do not know how to resolve TermParameter in " + owner)
        }
      case Symbol.Global(qual, Signature.Method(name, jvmSignature)) =>
        val owner = loop(qual)
        val all = owner.info match {
          case x: g.OverloadedType => x.alternatives.flatMap(_.info.members)
          case x: g.Type => x.members
        }
        val candidates = all.filter(x => x.isMethod && x.nameString == name)

        val res = candidates
          .find { x =>
            val meta = toMeta(x)
            if (meta.isInstanceOf[Symbol.Global]) {
              val asSymbol = meta.asInstanceOf[Symbol.Global]
              val signature = asSymbol.signature
              if (signature.isInstanceOf[Signature.Method]) {
                val asMethodSignature = signature.asInstanceOf[Signature.Method]
                asMethodSignature.jvmSignature == jvmSignature
              } else {
                false
              }
            } else {
              false
            }
          }
          .get

        res
      case _ =>
        throw new RuntimeException(symbol.structure)
    }

    Try {
      val gsym = loop(symbol)

      if (gsym == g.NoSymbol) {
        throw new NoSuchElementException(symbol.syntax)
      }

      gsym
    }
  }
  implicit class CleanSymbol(symbol: Symbol) {
    val cleanOwner = Cleaners.cleanOwner(
      symbol
        .productElement(0)
        .toString)
    val name = symbol.productElement(1).toString.split("""\(""").head
    val cleanName = Cleaners.cleanName(name)
    val cleanWhole =
      if (cleanOwner != "") s"$cleanOwner.$cleanName"
      else cleanName
  }

  implicit class FindableSymbol(symbol: Symbol.Global) {
    val isInAnonScope = symbol.owner match {
      case o: Symbol.Global => o.signature.name.startsWith("$anon")
      case _ => false
    }

    val isTerm = symbol.signature.isInstanceOf[Signature.Term]
    val isType = symbol.signature.isInstanceOf[Signature.Type]
    val isMethod = symbol.signature.isInstanceOf[Signature.Method]
    val isTermParameter = symbol.signature.isInstanceOf[Signature.TermParameter]
    val isTypeParameter = symbol.signature.isInstanceOf[Signature.TypeParameter]
    val isSelf = symbol.signature.isInstanceOf[Signature.Self]
  }


  def analyzeChecked: ImplicitAnalysisResult = {
    val callSites = {
      syntheticsWithImplicits
        .map(breakDownSynthetic)
        .map(reflectOnCallSite)
        .map(createCallSite)
    }

    val declarations = {
      inSourceDefinitions
        .flatMap(getDefn)
        .map(DeclarationReflection(_))
        .map(createDeclaration)
        .toSet
    }

    ImplicitAnalysisResult(callSites, declarations)
  }

  def analyze: ImplicitAnalysisResult = {
    val callSites = {
      syntheticsWithImplicits
        .map(syn => Try(createCallSite(reflectOnCallSite(breakDownSynthetic(syn)))))
        .reportAndExtract("CallSite")
    }

    val declarations = {
      inSourceDefinitions
        .map(t => Try(getDefn(t)))
        .reportAndExtract("Definition")
        .flatten
        .map(d => createDeclaration(DeclarationReflection(d)))
        .toSet
    }

    ImplicitAnalysisResult(callSites, declarations)
  }

  private implicit class TryCollection[A](from: Seq[Try[A]]) {
    def reportAndExtract(header: String): Seq[A] = {
      from.collect { case f@Failure(_) => f }.foreach(f => ErrorCollection().report(header, f.exception))
      from.collect { case Success(t) => t }
    }
  }

  def getDefn(tree: Tree): Seq[DefnBreakdown] = {
    def finder(t: Tree): Symbol = symbol(t).get

    DefnDecomposer(tree)(finder)
  }

  object DefnDecomposer {

    def handlePats(d: {def pats: Seq[Pat]})(
      implicit finder: Tree => Symbol): Seq[Symbol] = {
      assert(d.pats.size == 1,
        s"pattern Defn/Decl found with more than one pattern: $d")
      PatDecomposer(d.pats.head)
    }

    def handleTermName(d: {def name: Term.Name})(
      implicit finder: Tree => Symbol): Seq[Symbol] =
      Seq(finder(d.name))

    def handleTypeName(d: {def name: Type.Name})(
      implicit finder: Tree => Symbol): Seq[Symbol] =
      Seq(finder(d.name))

    def apply(defn: Tree)(
      implicit finder: Tree => Symbol): Seq[DefnBreakdown] = {
      val metaSymbols: Seq[Symbol] = (defn match {
        // Definitions
        case d: Defn.Val => handlePats(d)
        case d: Defn.Var => handlePats(d)
        case d: Defn.Def => handleTermName(d)
        case d: Defn.Macro => handleTermName(d)
        case d: Defn.Object => handleTermName(d)
        case d: Defn.Class => handleTypeName(d)
        case d: Defn.Type => handleTypeName(d)
        case d: Defn.Trait => handleTypeName(d)
        // Declarations
        case d: Decl.Val => handlePats(d)
        case d: Decl.Var => handlePats(d)
        case d: Decl.Def => handleTermName(d)
        case d: Decl.Type => handleTypeName(d)
        case d => throw new RuntimeException("Unknown defn symbol: " + d.toString())
      }) //.filterNot(x => SemanticCtx.isKnowCornerCase(defn, x))
      metaSymbols.map {
        case s: Symbol.Global =>
          Try(reflectOnDefn(s)) match {
            case Success(sym) =>
              DefnBreakdown(defn.pos, Some(sym), denotation(s))
            case Failure(e) => throw e
          }
        case s =>
          DefnBreakdown(defn.pos, None, denotation(s))
      }

      /*
        Try(ctx.reflectOnDefn(metaSymbol.asInstanceOf[Symbol.Global])) match {
          case Success(s) =>
            DefnBreakdown(
              pos = defn.pos,
              den = ctx.denotation(metaSymbol),
              sym = Some(s)
            )
          case Failure(x) if x.isInstanceOf[ClassCastException] =>
            DefnBreakdown(
              pos = defn.pos,
              den = ctx.denotation(metaSymbol),
              sym = None
            )
        }
        })*/
    }
  }

  def createCallSite(reflection: ImplicitReflection): s.CallSite = {
    val original = reflection.originalSymbol

    s.CallSite(
      location = createLocation(reflection.pos),
      name = reflection.fullName,
      code = reflection.code,
      isSynthetic = original.isSynthetic,
      declaration = createDeclaration(reflection.declaration),
      typeArguments = reflection.typeArguments.map(createTypeArgument),
      implicitArguments = reflection.args.map(createImplicitArgument)
    )
  }

  def createLocation(pos: Position = Position.None): Option[s.Location] = {
    pos match {
      case p: Position if p == Position.None => None
      case p => {
        val file: String = pos.input match {
          case Input.VirtualFile(path, _) => path
          case Input.File(path, _) => path.toString
          case _ => s"<unknown file: ${pos.input}"
        }
        Some(s.Location(file, pos.endLine, pos.endColumn))
      }
    }
  }

  /**
    * Get the parent representation of a class symbol.
    * TODO We assume for now that we only want a single level of inheritance, but it's easy to make it recursive
    */
  def createParent(child: DeclarationReflection,
                   parent: ParentReflection): s.Parent = {
    s.Parent(
      name = parent.fullName,
      declaration = s.Declaration(
        name = parent.declaration.fullName,
        kind = parent.declaration.kind,
        location = createLocation(parent.declaration.position),
        isImplicit = parent.declaration.isImplicit,
        signature = createSignature(parent.declaration),
        parents = Seq()
      ),
      typeArguments = parent.typeArguments.map(createTypeArgument)
    )
  }

  def createParam(symbol: g.Symbol): s.Parameter = {
    s.Parameter(
      name = symbol.name.toString,
      parameterType = createTypeParameter(symbol.typeSignature.typeSymbol.asType)
    )
  }

  def createParamList(paramList: List[g.Symbol]): s.ParameterList = {
    s.ParameterList(
      isImplicit = paramList.nonEmpty && paramList.head.isImplicit, // We assume that if one param is implicit, every param is
      parameters = paramList.map(createParam)
    )
  }

  def createSignature(reflection: DeclarationReflection): Option[s.Signature] = {

    val typeParams = reflection.typeSignature.typeParams.map(t =>
      createTypeParameter(t.asType))

    Some(
      s.Signature(
        typeParameters = typeParams,
        parameterLists = reflection.paramLists.map(createParamList),
        returnType = createTypeArgument(reflection.returnType)
      ))
  }

  def createDeclaration(reflection: DeclarationReflection): s.Declaration = {
    s.Declaration(
      name = reflection.fullName,
      kind = reflection.kind,
      location = createLocation(reflection.position),
      isImplicit = reflection.isImplicit,
      parents = reflection.baseClasses
        .map(createParent(reflection, _)),
      signature = createSignature(reflection)
    )
  }

  def createTypeParameter(tipe: g.TypeSymbol): s.Type = {
    s.Type(
      name = tipe.fullNameString,
      parameters = tipe.typeParams.map(t => createTypeParameter(t.asType))
    )
  }

  def createTypeArgument(t: g.Type): s.Type = {
    s.Type(t.toString, t.typeArgs.map(createTypeArgument))
  }

  def createTypeArgument(targ: ReflectiveTArg): s.Type = {
    s.Type(
      name = targ.fullName,
      parameters = targ.args.map(createTypeArgument)
    )
  }

  def createImplicitArgument(param: Param): s.Argument = {
    param match {
      case reflection: ImplicitReflection => {
        val original = reflection.originalSymbol
        s.Argument(
          code = reflection.code,
          info = Some(s.ArgumentInfo(
            name = reflection.fullName,
            declaration = createDeclaration(reflection.declaration),
            typeArguments = reflection.typeArguments.map(createTypeArgument),
            arguments = reflection.args.map(createImplicitArgument)
          ))
        )
      }
      case p: Param => {
        s.Argument(p.code)
      }
    }
  }

  def breakDownSynthetic(synth: Synthetic): CallSiteBreakDown = {
    def parse(text: String): Term = text.parse[Term].get

    def finder(tree: Tree): QualifiedSymbol = {
      synth.names.find(_.position.end == tree.pos.end) match {
        // Filter out the _star_ names
        // TODO: I don't think this case is relevant anymore, see processParamList()
        case Some(n) if n.symbol.syntax.contains("_star_") =>
          QualifiedSymbol.Empty
        case Some(name) =>
          QualifiedSymbol(Some(name.symbol), isSynthetic = true)
        case None => QualifiedSymbol.Empty
      }
    }

    val processedSynthetic = {
      val bd = TermDecomposer(parse(synth.text), finder).copy(
        pos = synth.position
      )
      CallSiteBreakDown(
        breakDown = bd,
        SyntheticOrigins(
          application = if (bd.symbol.app.isDefined) Some(synth) else None,
          paramList = Some(synth)
        )
      )
    }
    val res = processedSynthetic.breakDown.symbol.app match {
      case Some(app) => processedSynthetic
      case None => {
        val matchedApplication = findApplication(synth)
        //assertWeCanEraseParams(matchedApplication)
        matchedApplication.copy(
          matchedApplication.breakDown.copy(
            args = processedSynthetic.breakDown.args
          ),
          matchedApplication.origins.copy(
            paramList = processedSynthetic.origins.paramList
          )
        )
      }
    }
    assert(
      res.breakDown.symbol.app.isDefined,
      s"Couldn't find an application for synthetic ${synth.text}"
    )
    res
  }

  /**
    * Find applications for the synthetics that don't have them (that is, pure parameter lists)
    * For that, we try to look for a fitting `apply` synthetic. If we don't find one, we look in
    * the source code and try to match there.
    *
    * In both cases, we match by position, since parameter lists are inserted at the end of calls.
    *
    * We assume that there is exactly one symbol at the position of the synthetic.
    */
  def findApplication(synth: Synthetic): CallSiteBreakDown = {

    def breakdownTree(term: Tree): BreakDown = {
      def finder(t: Tree): QualifiedSymbol = {
        val sym = symbol(t)
        // A symbol from the tree will never be synthetic
        t match {
          // Special case: https://github.com/PRL-PRG/scalafix-rule-workshop/issues/39
          case tree: Term.Name
            if sym.isDefined &&
              sym.get.isInstanceOf[Symbol.Local] =>
            QualifiedSymbol(unrecurse(tree), isSynthetic = false)
          case tree =>
            QualifiedSymbol(
              Some(
                sym.getOrElse(
                  Symbol(qualifiedName(t.asInstanceOf[Term])))),
              isSynthetic = false
            )
        }
      }

      term match {
        case t: Init => InitDecomposer(term.asInstanceOf[Init], finder)
        case t: Term => TermDecomposer(term.asInstanceOf[Term], finder)
        case _ => throw new RuntimeException("Unknown term: " + term.toString())
      }
    }

    syntheticApplication(synth.position.end) match {
      // There is a synthetic application that matches
      case Some(syntheticApply) => breakDownSynthetic(syntheticApply)
      // Parse from the tree itself
      case None =>
        CallSiteBreakDown(
          breakdownTree(
            inSourceCallSite(synth.position.end)
              .getOrElse {
                throw new RuntimeException(
                  s"No application found in source for ${synth.text}@${synth.position.endLine}:${synth.position.endColumn}")
              })
        )
    }

  }

  def reflectOnCallSite(what: CallSiteBreakDown): ImplicitReflection = {
    val metaSymbol = what.breakDown.symbol.app.get
    val reflectiveSymbol =
      Finders.findCallSiteSymbol(metaSymbol.asInstanceOf[Symbol.Global])

    ImplicitReflection(what.breakDown,
                       denotation(metaSymbol),
                       reflectiveSymbol,
                       what.origins)
  }

  def reflectOnArg(arg: Param, origin: Option[Synthetic]): Param = {
    arg match {
      case bd: BreakDown => {
        val metaSymbol = bd.symbol.app.get
        val reflectiveSymbol =
          Finders.findArgumentSymbol(metaSymbol.asInstanceOf[Symbol.Global])

        ImplicitReflection(bd,
                           denotation(metaSymbol),
                           reflectiveSymbol,
                           SyntheticOrigins(origin, origin))
      }
      case p: Param => p
    }
  }

  def reflectOnType(s: Symbol.Global): g.Symbol = {
    Finders.findTypeSymbol(s)
  }

  def reflectOnDefn(metaSymbol: Symbol.Global): g.Symbol = {
    Finders.findDefnSymbol(metaSymbol)
  }

  object Finders {
    def logAndThrow(what: String, sym: Symbol) = {
      val owner = sym match {
        case x: Symbol.Global => toGlobal(x.owner).map(_.kindString).getOrElse("Unknown owner")
        case _ => "Unknown owner"
      }

      val e = new RuntimeException(s"Could not find symbol for ${what} ${sym} (owner: $owner)")
      logger.error(s"Could not find symbol for ${what} ${sym}")
      throw e
    }

    def findCallSiteSymbol(metaSymbol: Symbol.Global): g.Symbol = {
      def tryLoad(attempts: List[Symbol.Global => Try[g.Symbol]], results: List[Try[g.Symbol]]): List[Try[g.Symbol]] = attempts match {
        case Nil => results
        case x :: xs => x(metaSymbol) match {
          case s@Success(_) => s :: results
          case s@Failure(_) => tryLoad(xs, s :: results)
        }
      }

      val all = tryLoad(List(loadClass, loadMethod, loadModule), List())
      val symbol = all collectFirst { case Success(s) => s }

      // TODO: for debugging
      if (symbol.isDefined) {
        symbol.get
      } else {
        logAndThrow("call site", metaSymbol)
      }
    }

    def findDefnSymbol(metaSymbol: Symbol.Global): g.Symbol = {
      val x = toGlobal(metaSymbol)

      if (x.isFailure) {
        logAndThrow(s"defn symbol - isAnon:${metaSymbol.isInAnonScope}", metaSymbol)
      } else {
        x.get
      }
    }

    def findArgumentSymbol(metaSymbol: Symbol.Global): g.Symbol = {
      val x = toGlobal(metaSymbol)

      if (x.isFailure) {
        logAndThrow(s"argument - isAnon:${metaSymbol.isInAnonScope}", metaSymbol)
      } else {
        x.get
      }
    }

    def findTypeSymbol(metaSymbol: Symbol.Global): g.Symbol = {
      val x = toGlobal(metaSymbol)

      if (x.isFailure) {
        logAndThrow(s"type symbol - isAnon:${metaSymbol.isInAnonScope}", metaSymbol)
      } else {
        x.get
      }
    }

    //      if (metaSymbol.isTypeParameter) {
    //        loadTypeParameter(metaSymbol) match {
    //          case Success(s) => s
    //          case Failure(_) => logAndThrow("type symbol param", metaSymbol)
    //        }
    //      } else {
    //        loadClass(metaSymbol) match {
    //          case Success(t) => t
    //          case Failure(ex) =>
    //            loadPackage(metaSymbol) match {
    //              case Success(t) => t
    //              case Failure(ex) =>
    //                loadModule(metaSymbol) match {
    //                  case Success(t) => t.moduleClass
    //                  case Failure(ex) =>
    //                    // For some types, like String, scala aliases java.lang.String.
    //                    // The staticClass set of methods do recursive dealiasing, and probably
    //                    // can't handle Java classes
    //                    loadTypeMember(metaSymbol) match {
    //                      case Success(t) => t
    //                      case Failure(_) => logAndThrow("type symbol", metaSymbol)
    //                    }
    //                }
    //            }
    //        }
    //      }
    //    }
  }

  private def loadField(target: Symbol.Global, f: g.Symbol => Boolean): Try[g.Symbol] =
    toGlobal(target).filter(f)

  def loadAnyField(target: Symbol.Global): Try[g.Symbol] = loadField(target, _ => true)


  def loadParameter(symbol: Symbol.Global): Try[g.Symbol] = {
    // TODO: do we need this?
    // FIXME:
    loadAnyField(Symbol.Global(symbol.owner, Signature.Term(symbol.signature.name)))
    }

  def loadMethod(symbol: Symbol.Global): Try[g.Symbol] =
    toGlobal(symbol)

  def loadTypeMember(symbol: Symbol.Global): Try[g.Symbol] = {
      loadField(symbol, _.isType)
    }

  def loadTypeParameter(target: Symbol.Global): Try[g.Symbol] =
    toGlobal(target)

  // TODO: this is broken - it can resolve to a non-class symbol
  def loadClass(symbol: Symbol): Try[g.ClassSymbol] = symbol match {
    case s: Symbol.Global if s.isMethod && s.cleanName == "<init>" => toGlobal(s.owner).map(_.asClass)
    case s: Symbol.Global if s.isMethod => toGlobal(s).map(_.companionClass.asClass)
    case s: Symbol.Global => toGlobal(s).map(_.asClass)
    case s => Failure(new RuntimeException(s"Cannot load non-global symbol $s"))
  }

  def loadModule(symbol: Symbol): Try[g.ModuleSymbol] = symbol match {
    case s: Symbol.Global => toGlobal(s).map(_.asModule)
    case s => Failure(new RuntimeException(s"Cannot load non-global symbol ${s}"))
  }

  def loadPackage(symbol: Symbol): Try[g.ModuleSymbol] = toGlobal(symbol).map(_.asModule)

  object Cleaners {
    def cleanOwner(raw: String): String =
      raw
        .stripPrefix("_root_.")
        .stripPrefix("_empty_.")
        .stripSuffix(".")
        .stripSuffix("#")
        .replace("#", ".")

    def cleanName(raw: String): _root_.scala.Predef.String =
      raw
        .stripPrefix("[")
        .stripSuffix("]")
        .stripSuffix("#")
        .stripSuffix(".")
        .replace("`init`", "$init$")
        .replace("`", "")

    def separateLastPart(fullName: String): (String, String) = {
      val base = fullName.replace("#", ".")
      val lastName = base.split("""\.""").last
      val theRest = base.substring(0, fullName.lastIndexOf("."))
      (theRest, lastName)
    }
  }

  def getReflectiveKind(symbol: g.Symbol): String = {
    var kind: String = symbol match {
      case x if x.isMethod => "def"
      case x if x.isClass =>
        x.asClass match {
          case c if c.isTrait => "trait"
          case c if c.isCaseClass => "case class"
          case c if c.isPackage => "package"
          case c if c.isPackageClass => "package class"
          case c => "class"
        }
      case x if x.isTerm =>
        x.asTerm match {
          case t if t.isParameter => "param"
          case t if t.isVal => "val"
          case t if t.isVal => "var"
          case t if t.isModule => "object"
          case t if t.isPackage => "package"
          case t if t.toString.startsWith("value") => "value"
        }
      case x if x.isMacro => "macro"
      case x => throw new RuntimeException(s"<unknown: ${x.toString}>")
    }
    if (symbol.isFinal) kind = s"final $kind"
    if (symbol.isAbstract) kind = s"abstract $kind"
    if (symbol.isTerm) {
      if (symbol.asTerm.isLazy) kind = s"lazy $kind"
    }
    kind
  }

  def firstLevelBaseClasses(baseClasses: List[g.Symbol]): List[g.Symbol] = {
    // Take the tail because the first one is the self definition
    // Remove the classes that are parents of some class in bases
    baseClasses match {
      case bases if bases.isEmpty => List()
      case bases =>
        bases.tail.filterNot(cls =>
          bases.tail.exists(_.typeSignature.baseClasses.tail.contains(cls)))
    }
  }

  def returnType(ref: g.Symbol): g.Type = {
    ref match {
      case r if r.isMethod => r.asMethod.returnType
      case r if r.isClass => returnType(r.asClass.primaryConstructor)
      case r => r.typeSignature
    }
  }

  def paramLists(ref: g.Symbol): List[List[g.Symbol]] = {
    ref match {
      case r if r.isMethod => r.asMethod.paramLists
      case r if r.isConstructor => r.asMethod.paramLists
      case r if r.isClass => paramLists(r.asClass.primaryConstructor)
      case _ => List()
    }
  }

  /**
    * Intermediate representation that deals with incomplete information
    * (e.g. a missing Denotation) and gathers the information needed to
    * call the factories later.
    *
    * It could arguably be merged with the Factories, but that would make
    * the factories very complicated.
    */
  /**
    * The reflection for either an implicit CallSite or an ImplicitArgument in a call site.
    */
  case class ImplicitReflection(originalSymbol: QualifiedSymbol,
                                reflectiveSymbol: g.Symbol,
                                fullName: String,
                                pos: Position,
                                code: String,
                                declaration: DeclarationReflection,
                                typeArguments: Seq[ReflectiveTArg],
                                args: Seq[Param])
    extends Param

  object ImplicitReflection {
    def apply(bd: BreakDown,
              den: Option[Denotation],
              ref: g.Symbol,
              origins: SyntheticOrigins): ImplicitReflection =
      new ImplicitReflection(
        originalSymbol = bd.symbol,
        reflectiveSymbol = ref,
        fullName = ref.fullNameString,
        pos = bd.pos,
        declaration = DeclarationReflection(Position.None, ref, den),
        code = bd.code,
        args = bd.args.map(reflectOnArg(_, origins.paramList)),
        typeArguments = bd.targs.map(ReflectiveTArg(_, origins.application))
      )
  }

  case class ParentReflection(fullName: String,
                              kind: String,
                              declaration: DeclarationReflection,
                              typeArguments: Seq[ReflectiveTArg])

  object ParentReflection {
    def apply(pos: Position, tpe: g.Type): ParentReflection = {
      val sym = tpe.typeSymbol
      ParentReflection(
        fullName = sym.fullName,
        kind = getReflectiveKind(sym),
        declaration = DeclarationReflection(pos, sym, None),
        typeArguments = tpe.typeArgs.map(ReflectiveTArg(_))
      )
    }
  }

  case class DeclarationReflection(sym: g.Symbol,
                                   fullName: String,
                                   kind: String,
                                   position: Position,
                                   isImplicit: Boolean,
                                   baseClasses: List[ParentReflection],
                                   typeSignature: g.Type,
                                   paramLists: List[List[g.Symbol]],
                                   returnType: g.Type)

  object DeclarationReflection {
    def apply(pos: Position,
              sym: g.Symbol,
              denot: Option[Denotation]): DeclarationReflection =
      DeclarationReflection(
        sym = sym,
        fullName = sym.fullNameString,
        kind = denot match {
          case Some(d) => SemanticCtx.getKind(d)
          case None => getReflectiveKind(sym)
        },
        position = pos,
        isImplicit = denot match {
          case Some(d) => d.isImplicit
          case None => sym.isImplicit
        },
        baseClasses = baseClasses(pos, sym),
        typeSignature = sym.typeSignature,
        paramLists = paramLists(sym),
        returnType = returnType(sym)
      )

    def createFromLocal(pos: Position, denot: Denotation) =
      new DeclarationReflection(
        sym = g.NoSymbol,
        fullName = s"_local_${denot.name}${denot.signature}",
        kind = SemanticCtx.getKind(denot),
        position = pos,
        isImplicit = denot.isImplicit,
        baseClasses = List(),
        typeSignature = g.NoType,
        paramLists = List(List()),
        returnType = g.NoType
      )

    def apply(bd: DefnBreakdown): DeclarationReflection = {
      bd.sym match {
        case Some(s) => DeclarationReflection(bd.pos, s, bd.den)
        case None => createFromLocal(bd.pos, bd.den.get)
      }
    }

    def baseClasses(pos: Position, ref: g.Symbol): List[ParentReflection] = {
      firstLevelBaseClasses(ref.typeSignature.baseClasses)
        .map(ref.typeSignature.baseType(_))
        .map(ParentReflection(pos, _))
    }
  }

  case class ReflectiveTArg(fullName: String, args: Seq[ReflectiveTArg] = Seq())

  object ReflectiveTArg {

    def symbolName(t: Tree, symbol: Symbol): String = {
      def handleLocalTypeReference(ltr: Symbol.Global): String = {
        s"${
          Cleaners.cleanOwner(
            ltr.productElement(0).asInstanceOf[Symbol.Global].productElement(0).toString)
        }.${ltr.syntax}"
      }

      symbol match {
        case s: Symbol.Local => s"_local_.${t.syntax}"
        case s: Symbol.Global => {
          if (SemanticCtx.isLocalTypeReference(s)) handleLocalTypeReference(s)
          else reflectOnType(s).fullName // TODO: perhaps should just be a name
        }
        case s => throw new RuntimeException("Unknown symbol type " + symbol.toString)
      }
    }

    def processType(targ: Type)(
      implicit finder: Type => Symbol): ReflectiveTArg = targ match {
      case t: Type.Apply => {
        val pt = processType(t.tpe)
        val targs = t.args.map(processType)
        ReflectiveTArg(pt.fullName, targs)
      }
      case t: Type.Name => {
        val symbol = symbolName(t, finder(t))
        ReflectiveTArg(symbol)
      }
      case t: Type.Select => ReflectiveTArg(t.syntax)
      case t: Type.Refine => ReflectiveTArg(t.toString)
      case t: Type.Function => ReflectiveTArg(t.toString)
      case t: Type.Singleton => ReflectiveTArg(t.toString)
      case t =>
        throw new RuntimeException("Unknown type " + targ.toString())
    }

    def apply(targ: Type,
              synthSource: Option[Synthetic]): ReflectiveTArg = {
      def typeFinder(t: Type): Symbol = {
        synthSource match {
          // The application came from a synthetic
          case Some(synth) => {
            synth.names.find(_.position.end == t.pos.end) match {
              case Some(name) => name.symbol
              case None =>
                throw new MatchError(s"No name found in types for ${targ}")
            }
          }
          // We are looking for data from an application
          // that is in the source, so there is no synthetic
          case None => {
            names.find(_.position.end == t.pos.end).get.symbol
          }
        }
      }

      processType(targ)(typeFinder)
    }

    def apply(tpe: g.Type): ReflectiveTArg = ReflectiveTArg(
      fullName = tpe.typeSymbol.fullName,
      args = tpe.typeArgs.map(ReflectiveTArg(_))
    )
  }

  case class QualifiedSymbol(app: Option[Symbol], isSynthetic: Boolean)

  object QualifiedSymbol {
    val Empty = QualifiedSymbol(None, false)
  }

  // Break down of a CallSite or ImplicitArgument,
  // anything that has semantic meaning for us and is not
  // a declaration
  case class BreakDown(symbol: QualifiedSymbol,
                       targs: Seq[Type],
                       args: Seq[Param],
                       pos: Position,
                       code: String = "")
    extends Param

  case class SyntheticOrigins(application: Option[Synthetic] = None,
                              paramList: Option[Synthetic] = None)

  object SyntheticOrigins {
    lazy val Empty = SyntheticOrigins()
  }

  case class CallSiteBreakDown(breakDown: BreakDown,
                               origins: SyntheticOrigins = SyntheticOrigins.Empty)

  case class DefnBreakdown(pos: Position,
                           sym: Option[g.Symbol],
                           den: Option[Denotation])


  /**
    * Common interface for objects that decompose terms
    *
    * Walking a term tree to form a useful function
    * is a tedious process. This has to be done twice -
    * Once on the synthetics to examine the synthetic itself,
    * and another one once we have matched the synthetics with their missing applications.
    * This means that the only difference is in how they find symbols
    * (one finds them in the synthetic, the other in the code),
    * which why is this accepts a finder function.
    *
    * The accepted function is implicit for convenience, since breakDown and
    * processType are recursive and implicit functions simplify the calls.
    */
  object TermDecomposer {
    def apply(tree: Term, finder: Tree => QualifiedSymbol): BreakDown = {
      breakDown(tree)(finder)
    }

    def processParamList(params: Seq[Term])(
      implicit finder: Tree => QualifiedSymbol): Seq[Param] = {
      def getParameter(term: Tree): Param = term match {
        case t: Term.Assign => {
          RawCode(t.syntax, t.pos)
        }
        case t: Term.Block => {
          // A block inside the synthetic (e.g. `nested.this.a2c(*)({((a: A) => nested.this.a2b(a))})`)
          // We assume it has only one stat inside, and that it is a Term.
          // If it has more than one stat, we just regard it as a parameter
          if (t.stats.size == 1) {
            assert(t.stats.forall(_.isInstanceOf[Term]),
              s"Stat (${t.stats.head}) from block ${t} is not a term")
            getParameter(t.stats.head.asInstanceOf[Term])
          } else {
            RawCode(t.syntax, t.pos)
          }
        }
        case t: Term.Function => {
          // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
          // If the function appears in a parameter list, it is safe to consider it
          // as raw code, since implicit defs passed as implicit parameters will be passed in blocks
          // (See `case t: Term.Block`)
          t.body match {
            case application if SemanticCtx.isApplication(application) =>
              breakDown(application)
            case body => RawCode(t.syntax, t.pos)
          }
        }
        case t: Term.PartialFunction => {
          // A generated partial function application (e.g. `{ case (_, v) => v.head }`)
          RawCode(t.syntax, t.pos)
        }
        case t: Term.Ascribe => {
          // Type ascriptions: `((ClassTag.apply[String](classOf[java.lang.String])): ClassTag[String])`
          val app = getParameter(t.expr)
          app match {
            case app: BreakDown =>
              app.copy(targs = Seq(t.tpe), pos = t.pos, code = t.syntax)
            case app: Param => app
          }
        }
        case t: Term.Name => RawCode(t.syntax, t.pos)
        case t: Term.Placeholder => RawCode(t.syntax, t.pos)
        case t: Term.Interpolate => RawCode(t.syntax, t.pos)
        case t: Term.Repeated => RawCode(t.syntax, t.pos)
        case t: Term.Throw => RawCode(t.syntax, t.pos)
        case t: Term.Tuple => RawCode(t.syntax, t.pos)
        case t: Term.Ascribe => RawCode(t.syntax, t.pos)
        case t: Term.Eta => RawCode(t.syntax, t.pos)
        case t: Term.For => RawCode(t.syntax, t.pos)
        case t: Term.ForYield => RawCode(t.syntax, t.pos)
        case t: Term.If => RawCode(t.syntax, t.pos)
        case t: Term.Match => RawCode(t.syntax, t.pos)
        case t: Term.This => RawCode(t.syntax, t.pos)
        case t: Term.Try => RawCode(t.syntax, t.pos)
        case t: Term.TryWithHandler => RawCode(t.syntax, t.pos)
        case t: Term.While => RawCode(t.syntax, t.pos)
        case t: Term.Xml => RawCode(t.syntax, t.pos)
        case t: Lit.Int => RawCode(t.syntax, t.pos)
        case t: Lit.Symbol => RawCode(t.syntax, t.pos)
        case t: Lit => RawCode(t.syntax, t.pos)
        case t: Term => {
          val bd = breakDown(t)
          bd.symbol.app match {
            case Some(s) => bd
            case None => RawCode(t.syntax, t.pos)
          }
        }
        case t => RawCode(t.syntax, t.pos)
      }

      params
        .filterNot(x => {
          x.toString() == "*"
        })
        .map(getParameter)
    }

    private def breakDown(tree: Term)(
      implicit finder: Tree => QualifiedSymbol): BreakDown = {

      tree match {
        case t: Term.Apply => {
          // Anything with a parameter list (`hello(*)`, `hello[String](*)`, `hello[String](*)(stringConverter)`...)
          val bd = breakDown(t.fun)
          bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
        }
        case t: Term.ApplyType => {
          // An application with type parameters but no parameter list
          // examples: `test.this.JsonWriter[Seq[Student]]`
          val bd = breakDown(t.fun)
          bd.copy(targs = t.targs, pos = t.pos, code = t.syntax)
        }
        case t: Term.ApplyInfix => {
          // Infix function applications (e.g. `list map f1`).
          // We take the operation and the ''implicit'' parameters
          val bd = breakDown(t.op)
          bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
        }
        case t: Term.ApplyUnary => {
          // Unary functions
          val bd = breakDown(t.op)
          bd.copy(args = processParamList(Seq(t.arg)),
            pos = t.pos,
            code = t.syntax)
        }
        case t: Term.Select => {
          // Does not have parameters (otherwise it would be a Term.Apply) or type parameters (Term.ApplyType)
          // examples: `test.this.JsonWriter`
          breakDown(t.name).copy(pos = t.pos, code = t.syntax)
        }
        case t: Term.Name => {
          // Plain name of the symbol we want (e.g. in `test.this.JsonWriter` -> `"JsonWriter"`)
          val app = finder(t)
          BreakDown(app, Seq(), Seq(), t.pos)
        }
        case t: Term.New => {
          // Class constructor calls.
          // If the constructor is anonymous, we return the symbol of the class.
          // FIXME: This assumes that we won't find this in a synthetic
          val (app, targs) = t.init.name match {
              // If new always is anonymous, we can forgo this first match
            case n: Name.Anonymous => t.init.tpe match {
              case tpe: Type.Apply => finder(tpe.tpe) -> tpe.args
              case tpe: Type.Name => finder(tpe) -> Seq()
              case tpe: Type => {
                throw new MatchError(s"We don't handle type ${tpe} when breaking down constructor call ${t} @ ${t.pos}")
              }
            }
            case n: Name => {
              throw new MatchError(s"When analyzing Term.New, we don't know how to handle calls like ${t} @ ${t.pos}")
            }
          }
          BreakDown(app, targs, Seq(), pos = t.pos, code = t.syntax)
        }
        case t: Term.NewAnonymous => {
          // Anonymously constructed objects:
          // `new SnapshotFixtures { val buffer = // ... }`
          // We assume we have only one Init
          assert(t.templ.inits.size == 1,
            s"More than one init found for NewAnonymous ${t}")
          InitDecomposer(t.templ.inits.head, finder)
        }
        case t: Term.Function => {
          // A generated function (e.g. `(a: A) => nested.this.a2b(a)`)
          // We assume that the body is a single function call, as is the most typical in passing parameters
          // We also ignore the parameters for now, since they will appear in other call sites,
          // when the function gets executed
          breakDown(t.body)
        }
        case t: Term.Interpolate => {
          val bd = breakDown(t.prefix)
          bd.copy(args = processParamList(t.args), pos = t.pos, code = t.syntax)
        }
        case t => {
          throw new MatchError(s"Unknown Term match ${t.structure}")
        }
      }
    }
  }

  object InitDecomposer {
    def apply(init: Init, finder: Tree => QualifiedSymbol): BreakDown =
      BreakDown(
        symbol = finder(init.name),
        targs = Seq(init.tpe), //TODO not entirely sure about this, it will appear like [fun[Targs]]
        args = TermDecomposer.processParamList(init.argss.flatten)(finder),
        pos = init.pos
      )
  }

}

object ReflectiveCtx {
  def newCompiler(classpath: String, scalacOptions: List[String], usejavacp: Boolean): Global = {
    val vd = new VirtualDirectory("(memory)", None)
    val settings = new Settings
    settings.outputDirs.setSingleOutput(vd)
    settings.classpath.value = classpath
    if (classpath.isEmpty || usejavacp) {
      settings.usejavacp.value = true
    }
    settings.processArgumentString(
      ("-Ypresentation-any-thread" :: scalacOptions).mkString(" ")
    )
    val compiler = new Global(settings, new StoreReporter)

    // TODO: do we need this?
    //new SemanticdbPlugin(compiler) // hijack reporter/analyzer

    compiler.rootMirror.RootPackage.info.members
    compiler
  }
}
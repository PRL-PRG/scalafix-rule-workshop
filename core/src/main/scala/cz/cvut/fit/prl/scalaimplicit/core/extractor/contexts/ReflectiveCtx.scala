package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import java.net.URL

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx.Cleaners
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.Signature
import org.langmeta.inputs.Position
import org.langmeta.semanticdb.ResolvedName
import org.langmeta.semanticdb.Signature._

import scala.meta.{Database, Denotation, Symbol, Synthetic}
import scala.reflect.runtime.{universe => u}
import scala.util.{Failure, Success, Try}

class ReflectiveCtx(loader: ClassLoader, db: Database) extends SemanticCtx(db) {
  val _mirror = u.runtimeMirror(loader)

  implicit class FindableSymbol(symbol: Symbol.Global) {
    val cleanOwner = Cleaners.cleanOwner(
      symbol
        .productElement(0)
        .toString)
    val name = symbol.productElement(1).toString.split("""\(""").head
    val cleanName = Cleaners.cleanName(name)
    val cleanWhole = s"$cleanOwner.$cleanName"

    val isInAnonScope = symbol.owner
      .asInstanceOf[Symbol.Global]
      .signature
      .name
      .startsWith("$anon")

    val isTerm = symbol.signature.isInstanceOf[Term]
    val isType = symbol.signature.isInstanceOf[Type]
    val isMethod = symbol.signature.isInstanceOf[Method]
    val isTermParameter = symbol.signature.isInstanceOf[TermParameter]
    val isTypeParameter = symbol.signature.isInstanceOf[TypeParameter]
    val isSelf = symbol.signature.isInstanceOf[Self]
  }

  def reflectOnCallSite(what: CallSiteBreakDown): CallSiteReflection = {
    val metaSymbol = what.breakDown.symbol.app.get
    val reflectiveSymbol =
      Finders.findCallSiteSymbol(metaSymbol.asInstanceOf[Symbol.Global])
    CallSiteReflection(this,
                       what.breakDown,
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
        CallSiteReflection(this,
                           bd,
                           denotation(metaSymbol),
                           reflectiveSymbol,
                           SyntheticOrigins(origin, origin))
      }
      case p: Param => p
    }
  }

  def reflectOnType(s: Symbol.Global): u.Symbol = {
    Finders.findTypeSymbol(s)
  }

  def reflectOnDefn(metaSymbol: Symbol.Global): u.Symbol = {
    Finders.findDefnSymbol(metaSymbol)
  }

  object Finders {
    def logAndThrow(what: String, sym: Symbol) = {
      logger.error(s"Could not find symbol for ${what} ${sym}")
      throw new RuntimeException(s"Could not find symbol for ${what} ${sym}")
    }

    def findCallSiteSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      def tryImplicitClass(metaSymbol: Symbol.Global): Try[u.Symbol] = {
        Try(
          Loaders
            .loadClass(metaSymbol)
            .getOrElse( // Load class as a whole (it's inside an object)
              Loaders
                .loadClass(metaSymbol) // Load owner class and look into it
                .get
                .typeSignature
                .decls
                .sorted
                .find(_.name.toString == metaSymbol.cleanName)
                .get)
            .asClass
        )
      }

      tryImplicitClass(metaSymbol) match { // Impl
        case Success(s) => s
        case Failure(e) =>
          classOrObjectOrPackageMember(metaSymbol)
            .getOrElse(logAndThrow("call site", metaSymbol))

      }
    }

    def findDefnSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      //println(metaSymbol)
      Loaders.loadClass(metaSymbol) match { // class, trait, case class
        case Success(s) => s
        case Failure(_) =>
          Loaders.loadModule(metaSymbol) match {
            case Success(s) => s.asTerm // object
            case Failure(ex) => // val, var, def
              metaSymbol match {
                case s if s.isTerm || s.isType || s.isMethod =>
                  classOrObjectOrPackageMember(metaSymbol) match {
                    case Success(s) => s
                    case _ =>
                      if (metaSymbol.isInAnonScope) {
                        // Ugly special case: `a` in val aimpl = new A[Int] { def a(implicit v: Int): String = ??? }
                        classOrObjectOrPackageMember(
                          metaSymbol.owner
                            .asInstanceOf[Symbol.Global]
                            .owner
                            .asInstanceOf[Symbol.Global]).getOrElse(
                          logAndThrow("arg in anon scope", metaSymbol))
                      } else logAndThrow("arg", metaSymbol)
                  }
                case s if s.isTermParameter || s.isTypeParameter =>
                  classOrObjectOrPackageParameter(s) match {
                    case Success(s) => s
                    case _          => logAndThrow("arg", metaSymbol)
                  }
              }
          }
      }
    }

    def findArgumentSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      metaSymbol match {
        case s if s.isMethod => // def
          classOrObjectOrPackageMember(metaSymbol) match {
            case Success(s) if s.isMethod => s
            case _                        => logAndThrow("arg", metaSymbol)
          }
        case s if s.isTerm || s.isType => // object, val or var
          Loaders.loadModule(metaSymbol) match {
            case Success(s) => s.asTerm // object
            case Failure(ex) => // val or var
              classOrObjectOrPackageMember(metaSymbol) match {
                case Success(s) => s
                case _          => logAndThrow("arg", metaSymbol)
              }
          }
        case s if s.isTermParameter => // Things like (evidence$1)
          classOrObjectOrPackageParameter(s) match {
            case Success(s) => s
            case _          => logAndThrow("arg", metaSymbol)
          }
      }
    }
    def findTypeSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      Loaders.loadClass(metaSymbol) match {
        case Success(t) => t
        case Failure(ex) =>
          Loaders.loadPackage(metaSymbol) match {
            case Success(t) => t
            case Failure(ex) =>
              Loaders.loadModule(metaSymbol) match {
                case Success(t)  => t.moduleClass
                case Failure(ex) =>
                  // For some types, like String, scala aliases java.lang.String.
                  // The staticClass set of methods do recursive dealiasing, and probably
                  // can't handle Java classes
                  classOrObjectOrPackageTypeMember(metaSymbol) match {
                    case Success(t) => t
                    case Failure(_) => logAndThrow("type symbol", metaSymbol)
                  }
              }
          }
      }
    }

    private def classOrObjectOrPackageParameter(
        metaSymbol: Symbol.Global): Try[u.Symbol] = {
      Try({
        val param = metaSymbol.signature.asInstanceOf[TermParameter].name
        val o = metaSymbol.owner.asInstanceOf[Symbol.Global]
        classMember(o, param).getOrElse(
          objectMember(o, param).getOrElse(packageMember(o, param).get))
      })
    }

    private def classOrObjectOrPackageMember(
        metaSymbol: Symbol.Global): Try[u.Symbol] = {
      Try({
        val o = metaSymbol.owner.asInstanceOf[Symbol.Global]
        classMember(o, metaSymbol.cleanName).getOrElse(
          objectMember(o, metaSymbol.cleanName)
            .getOrElse(packageMember(o, metaSymbol.cleanName).get))
      })

    }

    private def classOrObjectOrPackageTypeMember(
        metaSymbol: Symbol.Global): Try[u.Symbol] = {
      val name = u.TypeName(metaSymbol.cleanName)
      Try({
        val o = metaSymbol.owner.asInstanceOf[Symbol.Global]
        classMember(o, name)
          .getOrElse(
            objectMember(o, name)
              .getOrElse(packageMember(o, name).get))
      })
    }

    private def classMember(owner: Symbol.Global,
                            name: String): Try[u.Symbol] = {
      Loaders
        .loadClass(owner)
        .map(
          ownerSymbol =>
            ownerSymbol.typeSignature.members.sorted
              .find(_.toString.endsWith(name))
              .get)
    }

    private def objectMember(owner: Symbol.Global,
                             name: String): Try[u.Symbol] = {
      Loaders
        .loadModule(owner)
        .map(
          ownerSymbol =>
            ownerSymbol.asModule.moduleClass.typeSignature.members.sorted
              .find(_.toString.endsWith(name))
              .get)
    }

    private def packageMember(owner: Symbol.Global,
                              name: String): Try[u.Symbol] = {
      Loaders
        .loadPackage(owner)
        .map(
          ownerSymbol =>
            ownerSymbol.asModule.moduleClass.typeSignature.members.sorted
              .find(_.toString.endsWith(name))
              .get)
    }

    private def classMember(owner: Symbol.Global,
                            name: u.TypeName): Try[u.Symbol] = {
      Loaders
        .loadClass(owner)
        .map(owner => owner.typeSignature.member(name))
    }

    private def objectMember(owner: Symbol.Global,
                             name: u.TypeName): Try[u.Symbol] = {
      Loaders
        .loadModule(owner)
        .map(owner => owner.asModule.moduleClass.typeSignature.member(name))
    }

    private def packageMember(owner: Symbol.Global,
                              name: u.TypeName): Try[u.Symbol] = {
      Loaders
        .loadPackage(owner)
        .map(owner => owner.asModule.moduleClass.typeSignature.member(name))
    }
  }

  object Loaders {

    def loadClass(symbol: Symbol): Try[u.ClassSymbol] =
      symbol match {
        case s: Symbol.Global =>
          s.owner match {
            case o: Symbol.Global if o.isType =>
              loadClass(o).map(
                x =>
                  x.typeSignature.members.sorted
                    .find(_.toString.endsWith(s.cleanName))
                    .get
                    .asClass)
            case _ => Try(_mirror.staticClass(s.cleanWhole))
          }
        case s =>
          Failure(new RuntimeException(s"Cannot load non-global symbol ${s}"))
      }

    def loadModule(symbol: Symbol.Global): Try[u.ModuleSymbol] =
      Try(_mirror.staticModule(symbol.cleanWhole))

    def loadPackage(symbol: Symbol.Global): Try[u.ModuleSymbol] =
      Try(_mirror.staticPackage(symbol.cleanWhole))
  }
}

object ReflectiveCtx {
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

  def getReflectiveKind(symbol: u.Symbol): String = {
    var kind: String = symbol match {
      case x if x.isMethod => "def"
      case x if x.isClass =>
        x.asClass match {
          case c if c.isTrait        => "trait"
          case c if c.isCaseClass    => "case class"
          case c if c.isPackage      => "package"
          case c if c.isPackageClass => "package class"
          case c                     => "class"
        }
      case x if x.isTerm =>
        x.asTerm match {
          case t if t.isParameter                  => "param"
          case t if t.isVal                        => "val"
          case t if t.isVal                        => "var"
          case t if t.isModule                     => "object"
          case t if t.isPackage                    => "package"
          case t if t.toString.startsWith("value") => "value"
        }
      case x if x.isMacro => "macro"
      case x              => throw new RuntimeException(s"<unknown: ${x.toString}>")
    }
    if (symbol.isFinal) kind = s"final $kind"
    if (symbol.isAbstract) kind = s"abstract $kind"
    if (symbol.isTerm) {
      if (symbol.asTerm.isLazy) kind = s"lazy $kind"
    }
    kind
  }

  def firstLevelBaseClasses(baseClasses: List[u.Symbol]): List[u.Symbol] = {
    // Take the tail because the first one is the self definition
    // Remove the classes that are parents of some class in bases
    baseClasses match {
      case bases if bases.isEmpty => List()
      case bases =>
        bases.tail.filterNot(cls =>
          bases.tail.exists(_.typeSignature.baseClasses.tail.contains(cls)))
    }
  }

  def returnType(ref: u.Symbol): u.Type = {
    ref match {
      case r if r.isMethod => r.asMethod.returnType
      case r if r.isClass  => returnType(r.asClass.primaryConstructor)
      case r               => r.typeSignature
    }
  }

  def paramLists(ref: u.Symbol): List[List[u.Symbol]] = {
    ref match {
      case r if r.isMethod      => r.asMethod.paramLists
      case r if r.isConstructor => r.asMethod.paramLists
      case r if r.isClass       => paramLists(r.asClass.primaryConstructor)
      case _                    => List()
    }
  }
}

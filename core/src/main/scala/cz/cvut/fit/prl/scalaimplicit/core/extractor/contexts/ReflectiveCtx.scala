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

class ReflectiveCtx(loader: ClassLoader, db: Database)
    extends SemanticCtx(db) {
  val _mirror = u.runtimeMirror(loader)

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

    val isTerm = symbol.signature.isInstanceOf[Term]
    val isType = symbol.signature.isInstanceOf[Type]
    val isMethod = symbol.signature.isInstanceOf[Method]
    val isTermParameter = symbol.signature.isInstanceOf[TermParameter]
    val isTypeParameter = symbol.signature.isInstanceOf[TypeParameter]
    val isSelf = symbol.signature.isInstanceOf[Self]
  }

  def reflectOnCallSite(what: CallSiteBreakDown): ImplicitReflection = {
    val metaSymbol = what.breakDown.symbol.app.get
    val reflectiveSymbol =
      Finders.findCallSiteSymbol(metaSymbol.asInstanceOf[Symbol.Global])
    ImplicitReflection(this,
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
        ImplicitReflection(this,
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
        Loaders.loadClass(metaSymbol)
      }

      def tryMethod(metaSymbol: Symbol.Global): Try[u.Symbol] = {
        Loaders.loadMethod(metaSymbol)
      }

      tryImplicitClass(metaSymbol) match { // Impl
        case Success(s) => s
        case Failure(e) =>
          tryMethod(metaSymbol).getOrElse(logAndThrow("call site", metaSymbol))
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
                  Loaders.loadAnyField(metaSymbol) match {
                    case Success(s) => s
                    case _ =>
                      if (metaSymbol.isInAnonScope) {
                        // Ugly special case: `a` in val aimpl = new A[Int] { def a(implicit v: Int): String = ??? }
                        Loaders
                          .loadAnyField(
                            metaSymbol.owner
                              .asInstanceOf[Symbol.Global]
                              .owner
                              .asInstanceOf[Symbol.Global])
                          .getOrElse(
                            logAndThrow("arg in anon scope", metaSymbol))
                      } else logAndThrow("arg", metaSymbol)
                  }
                case s if s.isTermParameter || s.isTypeParameter =>
                  Loaders.loadParameter(s) match {
                    case Success(s) => s
                    case _ => logAndThrow("arg", metaSymbol)
                  }
              }
          }
      }
    }

    def findArgumentSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      metaSymbol match {
        case s if s.isMethod => // def
          Loaders.loadMethod(metaSymbol) match {
            case Success(s) if s.isMethod => s
            case _ => logAndThrow("arg", metaSymbol)
          }
        case s if s.isTerm || s.isType => // object, val or var
          Loaders.loadModule(metaSymbol) match {
            case Success(s) => s.asTerm // object
            case Failure(ex) => // val or var
              Loaders.loadAnyField(metaSymbol) match {
                case Success(s) => s
                case _ => logAndThrow("arg", metaSymbol)
              }
          }
        case s if s.isTermParameter => // Things like (evidence$1)
          Loaders.loadParameter(s) match {
            case Success(s) => s
            case _ => logAndThrow("arg", metaSymbol)
          }
      }
    }

    def findTypeSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      if (metaSymbol.isTypeParameter) {
        Loaders.loadTypeParameter(metaSymbol) match {
          case Success(s) => s
          case Failure(_) => logAndThrow("type symbol param", metaSymbol)
        }
      } else {
        Loaders.loadClass(metaSymbol) match {
          case Success(t) => t
          case Failure(ex) =>
            Loaders.loadPackage(metaSymbol) match {
              case Success(t) => t
              case Failure(ex) =>
                Loaders.loadModule(metaSymbol) match {
                  case Success(t) => t.moduleClass
                  case Failure(ex) =>
                    // For some types, like String, scala aliases java.lang.String.
                    // The staticClass set of methods do recursive dealiasing, and probably
                    // can't handle Java classes
                    Loaders.loadTypeMember(metaSymbol) match {
                      case Success(t) => t
                      case Failure(_) => logAndThrow("type symbol", metaSymbol)
                    }
                }
            }
        }
      }
    }
  }

  object Loaders {
    private def loadField(
        target: Symbol.Global,
        filterFunction: (u.Symbol => Boolean)): Try[u.Symbol] = {
      def searchInClass(s: Try[u.ClassSymbol]) =
        s.map(
          _.typeSignature.members.sorted
            .filter(filterFunction)
            .find(_.toString.endsWith(target.cleanName))
            .get)

      def searchInModule(s: Try[u.ModuleSymbol]) =
        s.map(
          _.moduleClass.info.members.sorted
            .filter(filterFunction)
            .find(_.toString.endsWith(target.cleanName))
            .get)

      Try(
        searchInClass(loadClass(target.owner)).getOrElse(
          searchInModule(loadModule(target.owner))
            .getOrElse(searchInModule(loadPackage(target.owner)).get)))
    }

    def loadAnyField(target: Symbol.Global): Try[u.Symbol] = {
      loadField(target, { _ =>
        true
      })
    }

    def loadParameter(symbol: Symbol.Global): Try[u.Symbol] = {
      loadAnyField(Symbol.Global(symbol.owner, Term(symbol.signature.name)))
    }

    def loadMethod(symbol: Symbol.Global): Try[u.Symbol] = {
      loadField(symbol, (x => x.isMethod || x.isClass))
    }

    def loadTypeMember(symbol: Symbol.Global): Try[u.Symbol] = {
      loadField(symbol, _.isType)
    }

    def loadTypeParameter(target: Symbol.Global): Try[u.Symbol] = {
      def search(s: Try[u.Symbol]) =
        s.map(
          _.typeSignature.typeParams
            .find(_.toString.endsWith(target.cleanName))
            .get)

      Try(
        search(loadClass(target.owner)).getOrElse(
          search(loadModule(target.owner))
            .getOrElse(search(loadPackage(target.owner)).get)))
    }

    def loadClass(symbol: Symbol): Try[u.ClassSymbol] =
      symbol match {
        case s: Symbol.Global if s.isMethod && s.cleanName == "<init>" =>
          loadClass(s.owner)
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

    def loadModule(symbol: Symbol): Try[u.ModuleSymbol] =
      symbol match {
        case s: Symbol.Global =>
          s.owner match {
            case o: Symbol.Global if o.isType =>
              loadClass(o).map(
                x =>
                  x.typeSignature.members.sorted
                    .filter(_.isModule)
                    .find(_.toString.endsWith(s.cleanName))
                    .get
                    .asModule)
            case _ => Try(_mirror.staticModule(s.cleanWhole))
          }
        case s =>
          Failure(new RuntimeException(s"Cannot load non-global symbol ${s}"))
      }

    def loadPackage(symbol: Symbol): Try[u.ModuleSymbol] =
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
      case r if r.isClass => returnType(r.asClass.primaryConstructor)
      case r => r.typeSignature
    }
  }

  def paramLists(ref: u.Symbol): List[List[u.Symbol]] = {
    ref match {
      case r if r.isMethod => r.asMethod.paramLists
      case r if r.isConstructor => r.asMethod.paramLists
      case r if r.isClass => paramLists(r.asClass.primaryConstructor)
      case _ => List()
    }
  }
}

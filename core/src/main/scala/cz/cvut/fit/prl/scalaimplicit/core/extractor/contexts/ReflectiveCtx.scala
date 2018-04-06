package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx.Cleaners
import org.langmeta.semanticdb.Signature._

import scala.meta.{Database, Symbol, Synthetic}
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
      def tryImplicitClass(metaSymbol: Symbol.Global): Option[u.Symbol] = {
        Loaders.loadClass(metaSymbol)
      }

      def tryMethod(metaSymbol: Symbol.Global): Option[u.Symbol] = {
        Loaders.loadMethod(metaSymbol)
      }

      tryImplicitClass(metaSymbol) match { // Impl
        case Some(s) => s
        case _ =>
          tryMethod(metaSymbol).getOrElse(logAndThrow("call site", metaSymbol))
      }
    }

    def findDefnSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      //println(metaSymbol)
      Loaders.loadClass(metaSymbol) match { // class, trait, case class
        case Some(s) => s
        case _ =>
          Loaders.loadModule(metaSymbol) match {
            case Some(s) => s.asTerm // object
            case _ => // val, var, def
              metaSymbol match {
                case s if s.isTerm || s.isType || s.isMethod =>
                  Loaders.loadAnyField(metaSymbol) match {
                    case Some(s) => s
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
                            logAndThrow("defn in anon scope", metaSymbol))
                      } else logAndThrow("value defn", metaSymbol)
                  }
                case s if s.isTermParameter || s.isTypeParameter =>
                  Loaders.loadParameter(s) match {
                    case Some(s) => s
                    case _ => logAndThrow("param defn", metaSymbol)
                  }
              }
          }
      }
    }

    def findArgumentSymbol(metaSymbol: Symbol.Global): u.Symbol = {
      metaSymbol match {
        case s if s.isMethod => // def
          Loaders.loadMethod(metaSymbol) match {
            case Some(s) if s.isMethod => s
            case _ => logAndThrow("method arg", metaSymbol)
          }
        case s if s.isTerm || s.isType => // object, val or var
          Loaders.loadModule(metaSymbol) match {
            case Some(s) => s.asTerm // object
            case _ => // val or var
              Loaders.loadAnyField(metaSymbol) match {
                case Some(s) => s
                case _ => logAndThrow("term or type arg", metaSymbol)
              }
          }
        case s if s.isTermParameter => // Things like (evidence$1)
          Loaders.loadParameter(s) match {
            case Some(s) => s
            case _ => logAndThrow("termParameter (e.g. evidence$1) arg", metaSymbol)
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
          case Some(t) => t
          case _ =>
            Loaders.loadPackage(metaSymbol) match {
              case Some(t) => t
              case _ =>
                Loaders.loadModule(metaSymbol) match {
                  case Some(t) => t.moduleClass
                  case _ =>
                    // For some types, like String, scala aliases java.lang.String.
                    // The staticClass set of methods do recursive dealiasing, and probably
                    // can't handle Java classes
                    Loaders.loadTypeMember(metaSymbol) match {
                      case Some(t) => t
                      case None => logAndThrow("type symbol", metaSymbol)
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
                           filterFunction: (u.Symbol => Boolean)): Option[u.Symbol] = {
      def searchInClass(s: Option[u.ClassSymbol]): Option[u.Symbol] =
        s.flatMap(a => {
              val candidates =
                a.typeSignature.members.sorted
                 .filter(filterFunction)
                 .filter(_.toString.endsWith(target.cleanName))
            assert(candidates.size <= 1, s"Multiple candidates found for symbol ${target} in class ${s}")
            candidates.size match {
              case 1 => Some(candidates.head)
              case 0 => None
            }
          })

      def searchInModule(s: Option[u.ModuleSymbol]): Option[u.Symbol] =
        s.flatMap(a => {
            val candidates =
              a.moduleClass.info.members.sorted
                .filter(filterFunction)
                .filter(_.toString.endsWith(target.cleanName))
            assert(candidates.size <= 1, s"Multiple candidates found for symbol ${target} in class ${s}")
            candidates.size match {
              case 1 => Some(candidates.head)
              case 0 => None
            }
          })

      searchInClass(loadClass(target.owner)) match {
        case x : Some[u.Symbol] => x
        case None => searchInModule(loadModule(target.owner)) match {
          case x: Some[u.Symbol] => x
          case None => searchInModule(loadPackage(target.owner))
        }
      }
    }

    def loadAnyField(target: Symbol.Global): Option[u.Symbol] = {
      loadField(target, { _ =>
        true
      })
    }

    def loadParameter(symbol: Symbol.Global): Option[u.Symbol] = {
      loadAnyField(Symbol.Global(symbol.owner, Term(symbol.signature.name)))
    }

    def loadMethod(symbol: Symbol.Global): Option[u.Symbol] = {
      loadField(symbol, (x => x.isMethod || x.isClass))
    }

    def loadTypeMember(symbol: Symbol.Global): Option[u.Symbol] = {
      loadField(symbol, _.isType)
    }

    def loadTypeParameter(target: Symbol.Global): Try[u.Symbol] = {
      def search(s: Option[u.Symbol]) =
        s.map(
          _.typeSignature.typeParams
            .find(_.toString.endsWith(target.cleanName))
            .get)

      Try(
        search(loadClass(target.owner)).getOrElse(
          search(loadModule(target.owner))
            .getOrElse(search(loadPackage(target.owner)).get)))
    }

    def loadClass(symbol: Symbol): Option[u.ClassSymbol] =
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
            case _ => Try(_mirror.staticClass(s.cleanWhole)).toOption
          }
        case s =>
          throw new RuntimeException(s"Cannot load non-global symbol ${s}")
      }

    def loadModule(symbol: Symbol): Option[u.ModuleSymbol] =
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
            case _ => Try(_mirror.staticModule(s.cleanWhole)).toOption
          }
        case s =>
          throw new RuntimeException(s"Cannot load non-global symbol ${s}")
      }

    def loadPackage(symbol: Symbol): Option[u.ModuleSymbol] =
      Try(_mirror.staticPackage(symbol.cleanWhole)).toOption
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

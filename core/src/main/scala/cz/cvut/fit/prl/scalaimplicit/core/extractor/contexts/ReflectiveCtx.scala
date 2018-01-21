package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts

import java.net.URL

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.ReflectiveCtx.Cleaners
import cz.cvut.fit.prl.scalaimplicit.core.extractor.artifacts._
import org.langmeta.inputs.Position
import org.langmeta.semanticdb.ResolvedName

import scala.meta.{Database, Denotation, Symbol, Synthetic}
import scala.reflect.runtime.{universe => u}
import scala.util.{Failure, Success, Try}

class ReflectiveCtx(loader: ClassLoader, db: Database)
    extends SemanticCtx(db) {
  val _mirror = u.runtimeMirror(loader)

  def reflectOnBreakdown(x: SyntheticBreakdown): CallSiteReflection = {
    findReflection(x.breakDown, x.origins)
  }

  def findReflection(bd: BreakDown,
                     origins: SyntheticOrigins): CallSiteReflection = {
    val metaSymbol = bd.symbol.app.getOrElse(throw new RuntimeException(
      s"Breakdown ${bd.symbol} has no application and reached reflection. This should never happen"))
    val den = denotation(metaSymbol)
    val ref = findReflectSymbol(metaSymbol)
    if (den.isDefined) CallSiteReflection(this, bd, den.get, ref, origins)
    else CallSiteReflection(this, bd, ref, origins)
  }

  def reflectiveParam(param: Param, origin: Option[Synthetic]): Param = {
    param match {
      case bd: BreakDown =>
        findReflection(bd, SyntheticOrigins(origin, origin))
      case p: Param => p
    }
  }

  def findReflectSymbol(symbol: Symbol): u.Symbol = {
    object Finders {
      def findTypeSymbol(owner: String, tname: String) = {
        val name = s"$owner.$tname"
        getWholeSymbol(name).getOrElse(findName(owner, u.TypeName(tname)))
      }

      def findTypeParam(owner: String, tname: String): u.Symbol = {
        val (classContainer, className) = Cleaners.separateLastPart(owner)
        findName(classContainer, u.TypeName(className)).typeSignature.typeParams
          .find(_.fullName.endsWith(tname))
          .getOrElse(throw new RuntimeException(
            s"No type parameter matching $tname found in class $className"))
      }

      def findTerm(owner: String, name: String): u.Symbol =
        findName(owner, u.TermName(name))

      def findName(owner: String, name: u.Name): u.Symbol = {
        classMember(owner, name) match {
          case u.NoSymbol =>
            moduleMember(owner, name) match {
              case u.NoSymbol => packageMember(owner, name)
              case ms => ms
            }
          case cs => cs
        }
      }

      // Get a single symbol from an fqn
      def getWholeSymbol(fqn: String): Option[u.Symbol] = {
        val s =
          Loaders
            .loadClass(fqn)
            .getOrElse(
              Loaders
                .loadModule(fqn)
                .getOrElse(Loaders.loadPackage(fqn).getOrElse(u.NoSymbol)))
        s match {
          case u.NoSymbol => None
          case s => Some(s)
        }
      }

      def classMember(cname: String, name: u.Name): u.Symbol = {
        Loaders
          .loadClass(cname)
          .getOrElse(u.NoSymbol)
          .typeSignature
          .member(name)
      }
      def moduleMember(mname: String, name: u.Name): u.Symbol = {
        Loaders.loadModule(mname) match {
          case Some(m) => m.asModule.moduleClass.info.member(name)
          case None => u.NoSymbol
        }
      }
      def packageMember(pname: String, name: u.Name): u.Symbol = {
        Loaders.loadPackage(pname) match {
          case Some(p) => p.moduleClass.info.member(name)
          case None => u.NoSymbol
        }
      }
    }

    object Loaders {
      def loadClass(symbol: String): Option[u.ClassSymbol] = {
        try {
          Some(_mirror.staticClass(symbol))
        } catch {
          case _: ScalaReflectionException => None
        }
      }
      def loadModule(symbol: String): Option[u.ModuleSymbol] = {
        try {
          Some(_mirror.staticModule(symbol))
        } catch {
          case _: ScalaReflectionException => None
        }
      }
      def loadPackage(symbol: String): Option[u.ModuleSymbol] = {
        try {
          Some(_mirror.staticPackage(symbol))
        } catch {
          // FIXME This is an umbrella to catch both ScalaReflectionException and java ReflectionError
          case _: Throwable => None
        }
      }
    }

    def isType(name: String) =
      name.isEmpty || name.endsWith("#") // In scalameta, symbols that end in # are type names
    def isTypeParam(name: String) = name.matches("""\[.*\]""")

    val owner = Cleaners.cleanOwner(
      symbol
        .productElement(0)
        .toString)
    val name = symbol.productElement(1).toString.split("""\(""").head
    val reflection: u.Symbol = (owner, name) match {
      case (o, n) if isType(n) =>
        Finders.findTypeSymbol(o, Cleaners.cleanName(n))
      case (o, n) if isTypeParam(n) =>
        Finders.findTypeParam(o, Cleaners.cleanName(n))
      case (o, n) =>
        Finders.findTerm(o, Cleaners.cleanName(n)) match {
          case u.NoSymbol =>
            Finders
              .getWholeSymbol(symbol.syntax)
              .getOrElse(throw new RuntimeException({
                s"Reflection for Term ${symbol} not found"
              }))
          case s => s
        }
    }
    assert(reflection != u.NoSymbol, {
      s"Reflection for Symbol $symbol not found"
    })
    reflection
  }

  /**
    * Helper method to determine whether a symbol from inside the project
    * The heuristics used are that if we have a class loaded from a file,
    * then it must be a source of the project.
    * @param ref
    * @return
    */
  def isExternal(ref: u.Symbol): Boolean = {
    /* Code commented out until we figure out how to properly get the URLs
    def getResource(s: String): URL = {
      _mirror.classLoader.getResource(s) match {
        case null =>
          throw new ClassNotFoundException(s"Resource for $s not found")
        case x => x
      }
    }

    def getNearestClass(symbol: u.Symbol): u.ClassSymbol = {
      assert(symbol != _mirror.RootPackage)
      symbol match {
        case s if s.isClass => s.asClass
        case s              => getNearestClass(s.owner)
      }
    }
    def getSelfType(symbol: u.Symbol): u.Type = {
      getNearestClass(symbol).selfType
    }
    // Try to get the url from a symbol, looking up owners recursively.
    // A symbol's owner is defined in the project iff the symbol is also defined in the project:
    // http://docs.scala-lang.org/overviews/reflection/symbols-trees-types.html#the-symbol-owner-hierarchy
    def tryOwnerChain(s: u.Symbol): URL = {
      assert(s != u.NoSymbol, s"URL not found for symbol ${ref}")
      Try(_mirror.runtimeClass(getSelfType(s))) match {
        case Success(runtimeClass) =>  getResource(parseFullName(runtimeClass.getCanonicalName))
        case Failure(exc) => tryOwnerChain(s.owner)
      }
    }
    // Convert a string to a plausible path name for the classloader
    def parseFullName(name: String): String = {
      val header = name.split("""\(""").head
      val i = header.lastIndexOf("#") match {
        case -1 => header.length; case x => x
      }
      header
        .substring(0, i)
        .stripSuffix("#")
        .replace("#", "/")
        .replace(".", "/")
        .concat(".class")
    }
    val url = tryOwnerChain(ref)
    url.getProtocol() != "file"
     */
    false
  }
}

object ReflectiveCtx {
  object Cleaners {
    def cleanOwner(raw: String): String =
      raw
        .stripPrefix("_empty_.")
        .stripPrefix("_root_.")
        .stripSuffix(".")
        .stripSuffix("#")
        .replace("#", ".")

    def cleanName(raw: String): _root_.scala.Predef.String =
      raw
        .stripPrefix("[")
        .stripSuffix("]")
        .stripSuffix("#")
        .stripSuffix(".")

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

  def returnType(ref: u.Symbol) = {
    ref match {
      case r if r.isMethod => r.asMethod.returnType
      case r => r.typeSignature
    }
  }

  def paramLists(ref: u.Symbol) = {
    ref match {
      case r if r.isMethod => r.asMethod.paramLists
      case _ => List()
    }
  }
}

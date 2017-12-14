package cz.cvut.fit.prl.scalaimplicit.extractor.contexts

import scala.meta.{Database, Symbol}

class ReflectiveCtx(loader: ClassLoader, db: Database)
    extends SemanticCtx(db) {
  import scala.reflect.runtime.{universe => u}
  val _mirror = u.runtimeMirror(loader)

  def findReflectSymbol(symbol: Symbol): u.Symbol = {
    object Finders {
      def findTypeSymbol(owner: String, tname: String) = {
        val name = s"$owner.$tname"
        getWholeSymbol(name).getOrElse(findName(owner, u.TypeName(tname)))
      }

      def findTypeParam(owner: String, tname: String): u.Symbol = {
        val ownerParts = owner.replace("#", ".").split(".")
        val className = ownerParts.last
        val classContainer = owner.replace(s".$className", "")
        val container = Loaders
          .loadClass(classContainer)
          .getOrElse(Loaders
            .loadModule(classContainer)
            .getOrElse(throw new RuntimeException(
              s"No suitable container for $className found in $classContainer")))
        container.typeSignature
          .member(u.TypeName(className))
          .typeSignature
          .typeParams
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

    object Cleaners {
      def cleanOwner(raw: String): String =
        raw
          .stripPrefix("_empty_.")
          .stripPrefix("_root_.")
          .stripSuffix(".")
          .stripSuffix("#")

      def cleanName(raw: String): _root_.scala.Predef.String =
        raw
          .stripPrefix("[")
          .stripSuffix("]")
          .stripSuffix("#")
          .stripSuffix(".")
    }

    def isType(name: String) =
      name.isEmpty || name.endsWith("#") // In scalameta, symbols that end in # are type names
    def isTypeParam(name: String) = name.matches("""\[.*\]""")

    val owner = Cleaners.cleanOwner(
      symbol
        .productElement(0)
        .toString)
    val name = symbol.productElement(1).toString.split("""\(""").head
    val reflection = (owner, name) match {
      case (o, n) if isType(n) =>
        Finders.findTypeSymbol(o, Cleaners.cleanName(n))
      case (o, n) if isTypeParam(n) =>
        Finders.findTypeParam(o, Cleaners.cleanName(n))
      case (o, n) => Finders.findTerm(o, Cleaners.cleanName(n))
    }
    assert(reflection != u.NoSymbol, s"Reflection for $symbol not found")
    reflection
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
          case t if t.isVal => "val"
          case t if t.isVal => "var"
          case t if t.isPackage => "package"
          case t if t.isModule => "object"
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
}

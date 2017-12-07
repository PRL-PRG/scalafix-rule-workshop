package cz.cvut.fit.prl.scalaimplicit.extractor.contexts

import scala.meta.{Database, Symbol}

class ReflectiveCtx(loader: ClassLoader, db: Database)
    extends SemanticCtx(db) {
  import scala.reflect.runtime.{universe => u}
  val _mirror = u.runtimeMirror(loader)

  /**
    * Fetch the possible scala.reflect representations of a scala.meta Symbol
    * @param symbol The scala.meta.Symbol whose representation we want
    * @return A set of possible symbols with the same name as `symbol`
    */
  def fetchReflectSymbol(symbol: Symbol): Set[u.Symbol] = {

    /**
      * Extract a scala-reflect compatible fqn from a symbol.
      * `searchWoleSymbol` will be true when we need to
      * statically load the whole symbol ''in addition'' to
      * searching in the members of the owner
      * @param symbol
      * @return
      */
    case class ReflectLoadable(owner: String,
                               term: String,
                               searchWholeSymbol: Boolean)
    def splitFqn(symbol: Symbol): ReflectLoadable = {
      val owner =
        symbol
          .productElement(0)
          .toString
          .stripPrefix("_empty_.")
          .stripPrefix("_root_.")
          .stripSuffix(".")
          .stripSuffix("#")
      val name =
        symbol
          .productElement(1)
          .toString
          .split("""\(""")
          .head
      val isType = name.endsWith("#") // In scalameta, symbols that end in # are type names
      val tryWhole = isType
      ReflectLoadable(owner,
                      name
                        .stripSuffix("#")
                        .stripSuffix("."),
                      tryWhole)
    }

    /**
      * Given an owner and a term name to search, search for it with the mirror.
      * scala.reflect does not have a way to tell whether an fqn is a package, module or class.
      * Therefore, we have to try to load a symbol as each of those. https://goo.gl/MzBoJF
      *
      * It also may be that `name` is not there when we load something as a class, but
      * it appears when whe load it as a module (or vice versa).
      * Therefore, we have to keep looking if we don't find it at first.
      * FIXME: Exception-based flow control is obviously bad, we should look for a better way.
      */
    object SymbolSearch extends (ReflectLoadable => Set[u.Symbol]) {
      def apply(loadable: ReflectLoadable): Set[u.Symbol] = {
        val candidates = (classMembers(loadable.owner) ++
          moduleMembers(loadable.owner) ++
          packageMembers(loadable.owner))
          .filter(_.name.toString == loadable.term)
          .toSet
        if (loadable.searchWholeSymbol)
          candidates ++ getSymbol(s"${loadable.owner}.${loadable.term}")
        else candidates
      }

      // Get a single symbol from an fqn
      private def getSymbol(fqn: String): Set[u.Symbol] = {
        val s =
          loadClass(fqn).getOrElse(
            loadModule(fqn).getOrElse(loadPackage(fqn).getOrElse(u.NoSymbol)))
        if (s == u.NoSymbol) Set()
        else Set(s)
      }

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

      def classMembers(symbol: String): Seq[u.Symbol] = {
        loadClass(symbol).getOrElse(u.NoSymbol).typeSignature.members.toSeq
      }
      def moduleMembers(symbol: String): Seq[u.Symbol] = {
        loadModule(symbol) match {
          case Some(s) => s.moduleClass.info.members.toSeq
          case None => Seq()
        }
      }
      def packageMembers(symbol: String): Seq[u.Symbol] = {
        loadPackage(symbol) match {
          case Some(s) => s.moduleClass.info.members.toSeq
          case None => Seq()
        }
      }
    }

    val loadable = splitFqn(symbol)
    val candidates = SymbolSearch(loadable)
    assert(candidates.nonEmpty,
           s"We were unable to find anything matching $loadable")
    candidates
  }
}

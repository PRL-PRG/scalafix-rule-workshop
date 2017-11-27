package cz.cvut.fit.prl.scalaimplicit.heuristics

import cz.cvut.fit.prl.scalaimplicit.extractor.SemanticCtx
import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest
import org.langmeta.semanticdb.Synthetic

class ClassificationTest extends SemanticdbTest {

  case class Wrappers(extensions: Seq[Synthetic], conversions: Seq[Synthetic])

  /**
    * A function that tries to classify the usage of a wrapper function, in either
    * a class extension, an implicit conversion, or other.
    * @param wrapperFunctions
    * @return
    */
  def classifyUsages(ctx: SemanticCtx,
                     wrapperFunctions: Seq[Synthetic]): Wrappers = {
    def isClassExtension(syn: Synthetic): Boolean = {
      val symbol = syn.names.find(_.symbol.toString != "_star_.").get
      val parts = symbol.symbol.toString.split("""(?<=.*)(\(|\))""")
      val fqfn = parts(0)
      val fqreturn = parts(2).replace(";.", "").replace("/", ".").substring(1)
      fqfn.endsWith(fqreturn)
    }

    def isConversion(ctx: SemanticCtx, syn: Synthetic): Boolean = {
      !isClassExtension(syn)
    }
    val extensions = wrapperFunctions.filter(isClassExtension)
    val conversions = wrapperFunctions.filter(!isClassExtension(_))

    Wrappers(extensions, conversions)
  }

  checkContext(
    "Mixture of implicit conversions",
    """
      |package a
      |object Definition {
      | implicit class Hello(s: String) { def hello = "Hello" + s }
      | implicit def Str2Int(s: String): Int = s.length
      | implicit def notAConversion(i: Int): Int = i + 1
      |}
      |object Usage {
      | import Definition._
      | println("World".hello)
      | val a: Int = 1
      | val b: Int = 12
      | val x = a to b
      | val y: Int = "User".hello
      |}
      |// Example from https://github.com/PRL-PRG/scalafix-rule-workshop/wiki/Patterns:-Implicit-Class-Extension
    """.trim.stripMargin, { ctx =>
      val usages = ctx.index.synthetics
        .filter(_.text.matches("""(\.?[\[\w\]]*)+\(\*\)"""))
      val wrappers = classifyUsages(ctx, usages)
      wrappers.extensions should have size 2
      wrappers.conversions should have size 2
    }
  )

}

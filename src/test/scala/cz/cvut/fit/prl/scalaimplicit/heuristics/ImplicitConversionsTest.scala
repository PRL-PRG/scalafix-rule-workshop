package cz.cvut.fit.prl.scalaimplicit.heuristics

import cz.cvut.fit.prl.scalaimplicit.extractor.{ExtractImplicits, SemanticCtx}
import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest
import org.langmeta.semanticdb.Synthetic
class ImplicitConversionsTest extends SemanticdbTest {

  /**
    * Describe a mechanism to extract implicit conversion definitions
    * using the information we already have.
    * @param ctx
    */
  def checkDefinitionFromExtraction(ctx: SemanticCtx) = {
    val res = ExtractImplicits(ctx)
    val implicitDefs = res.implicits
      .filter(_.kind == "def")
      // TODO: Make the tests in DeclaredImplcitisTest work, so that it works when uncommenting this
      //.filter(_.nargs == "1")
    implicitDefs
  }

  /**
    * Define some conditions that all usages of this pattern must use.
    * For now, the only heuristic is that these are **not** implicit class extensions.
    * That is, their return type and function names do not coincide
    *
    * @param ctx
    */
  def checkUsages(ctx: SemanticCtx) = {
    // See that the inserted call matches the expected fqn.of.constructor(*) pattern
    val usages = ctx.index.synthetics.filter(_.text.matches("""(\.?[\[\w\]]*)+\(\*\)"""))
    usages.foreach { usage =>
      // Check that the return type is similar to the name of the class
      val symbol = usage.names.find(_.symbol.toString != "_star_.").get
      val parts = symbol.symbol.toString.split("""(?<=.*)(\(|\))""")
      val fqfn = parts(0)
      val fqreturn = parts(2).replace(";.", "").replace("/", ".").substring(1)
      fqfn should not endWith fqreturn
    }
    usages
  }

  checkContext(
    "Scala uses implicit conversion for ints in ranges",
    """
      |package conversions
      |object toFunction {
      |  val a: Int = 1
      |  val b: Int = 12
      |  val x = a to b
      |}
      |// From a blog: http://tomjefferys.blogspot.cz/2011/11/implicit-conversions-in-scala.html
    """.trim.stripMargin, { ctx =>
    val usages = checkUsages(ctx)
    usages.size shouldBe 1
  })

  checkContext(
    "Example with complex numbers",
    """
      |package conversions
      |object ComplexImplicits {
      |  implicit def Double2Complex(value : Double) = new Complex(value,0.0)
      |  implicit def Tuple2Complex(value : Tuple2[Double,Double]) = new Complex(value._1,value._2);
      |}
      |import ComplexImplicits._
      |class Complex(val real : Double, val imag : Double) {
      |  def +(that: Complex) : Complex = (this.real + that.real, this.imag + that.imag)
      |  override def toString = real + " + " + imag + "i"
      |}
      |
      |object Usage {
      | val c1 = new Complex(2.0, 4.0)
      | val a: Double = 4.0
      | val b: Double = 5.0
      | val c2 = a + c1
      | val c3 = (a, b) + c1
      |}
      |// From a blog: http://tomjefferys.blogspot.cz/2011/11/implicit-conversions-in-scala.html
    """.trim.stripMargin, { ctx =>
    val defsFromExtraction = checkDefinitionFromExtraction(ctx)
    defsFromExtraction.size shouldBe 2
    val usages = checkUsages(ctx)
    usages.size shouldBe 3
  })
}

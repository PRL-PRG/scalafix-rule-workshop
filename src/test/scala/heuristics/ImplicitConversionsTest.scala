package heuristics

import framework.SemanticdbTest
class ImplicitConversionsTest extends SemanticdbTest {
  checkContext(
    "Simple example with ranges: http://tomjefferys.blogspot.cz/2011/11/implicit-conversions-in-scala.html",
    """
      |package conversions
      |object toFunction {
      |  val a: Int = 1
      |  val b: Int = 12
      |  val x = a to b
      |}
    """.trim.stripMargin, { ctx =>
      // TODO: Custom assertions on the context
  })

  checkContext(
    "From a blog: http://tomjefferys.blogspot.cz/2011/11/implicit-conversions-in-scala.html",
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
    """.trim.stripMargin, { ctx =>
      // TODO: Custom assertions on the context
  })
}

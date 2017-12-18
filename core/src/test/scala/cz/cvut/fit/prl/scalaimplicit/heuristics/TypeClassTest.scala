package cz.cvut.fit.prl.scalaimplicit.heuristics

import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest

class TypeClassTest extends SemanticdbTest {
  checkContext(
    "JsonableExample",
    """
      |package typeclass
      |object JsonTest {
      | // Interface
      | sealed trait Json
      | object Json{
      |  case class Str(s: String) extends Json
      |  case class Num(value: Double) extends Json
      | }
      | trait Jsonable[T]{
      |   def serialize(t: T): Json
      | }
      |
      | // Default implementations
      | object Jsonable{
      |   implicit object StringJsonable extends Jsonable[String]{
      |     def serialize(t: String) = Json.Str(t)
      |   }
      |   implicit object DoubleJsonable extends Jsonable[Double]{
      |      def serialize(t: Double) = Json.Num(t)
      |   }
      |   implicit object IntJsonable extends Jsonable[Int]{
      |     def serialize(t: Int) = Json.Num(t.toDouble)
      |   }
      | }
      |
      | // Usage
      | def convertToJson[T](x: T)(implicit converter: Jsonable[T]): Json = {
      |   converter.serialize(x)
      | }
      |
      | // Call site
      | object Convert {
      |   val i: Int = 3
      |   val d: Double = 4.0
      |   val s: String = "5"
      |   convertToJson(i)
      |   convertToJson(d)
      |   convertToJson(s)
      | }
      |}
      |// Example from Li Haoyi's blog: http://www.lihaoyi.com/post/ImplicitDesignPatternsinScala.html#type-class-implicits
    """.trim.stripMargin, { ctx =>
      // TODO: Custom assertions on the context
    }
  )

  checkContext(
    "NumberLike Example",
    """
      |package typeclass
      |object MathTest {
      | // Interface
      | trait NumberLike[T] {
      |   def plus(x: T, y: T): T
      |   def divide(x: T, y: Int): T
      |   def minus(x: T, y: T): T
      | }
      |
      | // Default implementations
      | object NumberLike {
      |  implicit object NumberLikeDouble extends NumberLike[Double] {
      |    def plus(x: Double, y: Double): Double = x + y
      |    def divide(x: Double, y: Int): Double = x / y
      |    def minus(x: Double, y: Double): Double = x - y
      |  }
      |  implicit object NumberLikeInt extends NumberLike[Int] {
      |    def plus(x: Int, y: Int): Int = x + y
      |    def divide(x: Int, y: Int): Int = x / y
      |    def minus(x: Int, y: Int): Int = x - y
      |  }
      | }
      |
      | // Usage
      | object Statistics {
      |  import NumberLike._
      |  def mean[T](xs: Vector[T])(implicit ev: NumberLike[T]): T =
      |    ev.divide(xs.reduce(ev.plus(_, _)), xs.size)
      | }
      |
      | // Call site
      | object CallSite {
      |   import Statistics._
      |   val numbers = Vector[Double](13, 23.0, 42, 45, 61, 73, 96, 100, 199, 420, 900, 3839)
      |   println(Statistics.mean(numbers))
      | }
      |}
      |// Example from Daniel Westheide's blog: http://danielwestheide.com/blog/2013/02/06/the-neophytes-guide-to-scala-part-12-type-classes.html
    """.trim.stripMargin, { ctx =>
      // TODO: Custom assertions on the context
    }
  )

}

package implicits

object MathCallChains {

  implicit val msg: String = "Message in a bottle"

  class Adder {
    def apply(y: Int)(implicit message: String/*, otherMessage: String*/) = new Multiplier(y)
  }

  class Multiplier(v: Int) {
    def x()(implicit message: String) = this
    def apply(firstFactor: Int)(implicit message: String) = v * firstFactor
  }

  val adder = new Adder
  val result1 = adder(4).x().x().apply(3) // equivalent to x.apply(4).apply(3)
  val result2 = adder.apply(4).apply(3)
  //val result3 = adder(4)(3) // This is not a call chain at all
  //val result4 = adder.apply(4)(3) // This is also not a call chain, the (3) is the implicit parameter list (which is wrong)
}

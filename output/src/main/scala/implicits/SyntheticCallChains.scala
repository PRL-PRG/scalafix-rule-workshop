package implicits

object SyntheticCallChains {

  implicit val number: Int = 2

  class Adder {
    def apply(y: Int)(implicit two: Int) = new Multiplier(y + two)
  }

  class Multiplier(v: Int) {
    def apply(firstFactor: Int)(implicit two: Int) = v * firstFactor * two
  }

  val adder = new Adder
  val result = adder(4)(3) // equivalent to x.apply(4).apply(3)
}

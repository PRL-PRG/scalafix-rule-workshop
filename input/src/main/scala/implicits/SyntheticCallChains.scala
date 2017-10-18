/*
rule = "class:implicits.ImplicitContextCSV"
*/
package implicits

object SyntheticCallChains {

  implicit val msg: String = "Message in a bottle"

  class C {
    def m1(y: Int)(implicit message: String) = this
    def m2(implicit message: String) = this
    def m3(implicit message: String) = this
    def apply(y: Int)(implicit message: String) = this
  }

  val c = new C
  c(1) // implicits.SyntheticCallChains.C.apply
  c.m1(1) // implicits.SyntheticCallChains.C.m1
  c.m1(1).m2 // implicits.SyntheticCallChains.C.m1; implicits.SyntheticCallChains.C.m2
  c.m3.m2.m1(1) // implicits.SyntheticCallChains.C.m3; implicits.SyntheticCallChains.C.m2; implicits.SyntheticCallChains.C.m1
}

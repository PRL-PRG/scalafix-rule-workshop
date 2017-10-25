/*
rule = "class:implicits.ImplicitContextCSV"
*/
package implicits

class ComplexArgument {
/*
  // Regular class and case classes
  case class CaseWriter(name: String, surname: String)
  class NormalWriter(nname: String, ssurname: String){
    val name: String = nname
    val surname: String = ssurname
  }
  implicit val arg: NormalWriter = new NormalWriter("Oscar", "Wilde")
  def normalWriter()(implicit writer: NormalWriter): String = writer.name
  normalWriter()
*/


  // Using typeclasses
  trait WriterLike[T, S] {
    def name: T
    def surname: T
    def birth: S
  }
  object WriterLike {
    implicit object WriterStringInt extends WriterLike[String, Int] {
      def name: String = "Edgar Allan"
      def surname: String = "Poe"
      def birth: Int = 1809
    }
  }

  class C {
    def m1[T, S](implicit writer: WriterLike[T, S]) = this
    def m2[T, S](implicit writer: WriterLike[T, S]) = this
    def m3[T, S](implicit writer: WriterLike[T, S]) = this
    def apply[T, S](implicit writer: WriterLike[T, S]) = this
  }

  val c = new C()
  c // implicits.SyntheticCallChains.C.apply
  c.m1 // implicits.SyntheticCallChains.C.m1
  c.m1.m2 // implicits.SyntheticCallChains.C.m1; implicits.SyntheticCallChains.C.m2
  c.m3.m2.m1 // implicits.SyntheticCallChains.C.m3; implicits.SyntheticCallChains.C.m2; implicits.SyntheticCallChains.C.m1


  //def typeclassWriter[T, S]()(implicit writer: WriterLike[T, S]) : T = writer.name
  //typeclassWriter()


}

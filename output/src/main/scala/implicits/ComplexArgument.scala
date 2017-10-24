package implicits

class ComplexArgument {

  // Regular class and case classes
  case class CaseWriter(name: String, surname: String)
  class NormalWriter(nname: String, ssurname: String){
    val name: String = nname
    val surname: String = ssurname
  }
  implicit val arg: NormalWriter = new NormalWriter("Oscar", "Wilde")
  def normalWriter()(implicit writer: NormalWriter): String = writer.name
  normalWriter()


  // Using typeclasses
  trait WriterLike[T] {
    def name: T
    def surname: T
  }
  object WriterLike {
    implicit object WriterLikeString extends WriterLike[String] {
      def name: String = "Oscar"
      def surname: String = "Wilde"
    }
  }
  def typeclassWriter[T]()(implicit writer: WriterLike[T]) : T = writer.name
  typeclassWriter()
}


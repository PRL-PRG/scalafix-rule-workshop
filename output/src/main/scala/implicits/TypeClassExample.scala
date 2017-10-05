package implicits

trait JSONConverter[T] {
  def convert(x: T): String
}

object Converters {
  implicit object StringJSONConverter extends JSONConverter[String] {
    override def convert(x: String): String = x
  }

  implicit object IntJSONConverter extends JSONConverter[Int] {
    override def convert(x: Int): String = x.toString
  }

  implicit def SeqJSONConverter[T : JSONConverter]: JSONConverter[Seq[T]] = (x: Seq[T]) =>
    x.map(implicitly[JSONConverter[T]].convert).mkString("{", ",", "}")
}

object JSONUtil {
  def convert[T](x:T)(implicit converter: JSONConverter[T]): String =
    converter.convert(x)

  def convert2[T: JSONConverter](x: T): String =
    implicitly[JSONConverter[T]].convert(x)
}

object JSONUtilTest {

  import Converters._

  def test(): Unit = {

    JSONUtil.convert(Seq("Hello", "World"))

  }
}
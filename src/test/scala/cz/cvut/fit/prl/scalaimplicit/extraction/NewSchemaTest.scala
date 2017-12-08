package cz.cvut.fit.prl.scalaimplicit.extraction

import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.extractor.ReflectExtract
import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest

class NewSchemaTest extends SemanticdbTest {

  checkReflContext(
    "",
    """
      |object test {
      |sealed trait Json
      |case class JsonObject(v: Map[String, Json]) extends Json
      |case class JsonArray(v: Seq[Json]) extends Json
      |case class JsonString(v: String) extends Json
      |
      |trait JsonConverter[A] {
      |  def convert(x: A): Json
      |}
      |
      |def write(x: Json): String = x match {
      |  case JsonObject(kvs) => (for ((k,v) <- kvs) yield k + ": " + write(v)).mkString("{", ", ", "}")
      |  case JsonArray(vs) => vs map write mkString("[", ", ", "]")
      |  case JsonString(v) => "'" + v + "'"
      |}
      |
      |def write[A](x: A)(implicit c: JsonConverter[A]): String = write(c.convert(x))
      |
      |case class Student(name: String)
      |
      |implicit object Student2Json extends JsonConverter[Student] {
      |  def convert(x: Student) = JsonObject(Map("name" -> JsonString(x.name)))
      |}
      |
      |implicit def seq2json[T: JsonConverter]: JsonConverter[Seq[T]] = new JsonConverter[Seq[T]] {
      |  def convert(x: Seq[T]) = JsonArray(x.map(implicitly[JsonConverter[T]].convert))
      |}
      |
      |implicit class JsonWriter[T : JsonConverter](that: T) {
      |  def toJson(): String = write(that)
      |}
      |
      |// 1.
      |Seq(Student("Alice")).toJson()
      |}
    """.stripMargin, { ctx =>
      val decl = Declaration(
        location = None,
        name = "test.this.JsonWriter",
        kind = "implicit class",
        isImplicit = true,
        signature = Some(
          Signature(
            typeParams = Seq(
              Type(
                name = "T",
                constraints = Some(": JsonConverter")
              )),
            parameterLists = Seq(
              DeclaredParameterList(
                isImplicit = false,
                params = Seq(
                  DeclaredParameter(
                    name = "",
                    tipe = Type("T")
                  ))
              ),
              DeclaredParameterList(
                isImplicit = true,
                params = Seq(
                  DeclaredParameter(
                    name = "evidence",
                    tipe = Type(
                      name = "T",
                      constraints = Some(": JsonConverter")
                    )
                  ))
              )
            ),
            returnType = Type(
              name = "JsonWriter",
              parameters = Seq(Type("T"))
            )
          ))
      )
      val call = CallSite(
        location = None,
        name = "test.this.JsonWriter",
        code =
          "test.this.JsonWriter[Seq[Student]](*)(test.this.seq2json[Student](test.this.Student2Json))",
        isSynthetic = true,
        declaration = decl,
        typeArguments = Seq(
          Type(
            name = "Seq",
            parameters = Seq(Type("Student"))
          )),
        implicitArguments = Seq(
          ImplicitArgument(
            name = "test.this.seq2json"
          ))
      )
      val res = ReflectExtract(ctx)
      res should contain only (call, decl)
      println("End")
    }
  )

  checkReflContext(
    "Heyo",
    """
      |object t {
      | case class A()
      | case class B()
      | implicit def b2a(b: B): A = new A()
      | def a(i: B)(implicit conv: B => A): A = conv(i)
      | val mb = new B()
      | a(mb)
      |}
    """.trim.stripMargin, { ctx =>
      println("End")
    }
  )

  checkReflContext(
    "Deep-nested calls",
    """
      |object nested {
      | case class A()
      | case class B()
      | case class C()
      | implicit def a2b(a: A): B = new B()
      | implicit def b2c(b: B): C = new C()
      | implicit def a2c(a: A)(implicit b: A => B, c: B => C): C = c(b(a))
      | val ma = new A()
      | val mc: C = ma
      |}
    """.trim.stripMargin, { ctx =>
      val res = ReflectExtract(ctx)
      println("End")
    }
  )

}

package cz.cvut.fit.prl.scalaimplicit.extraction

import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.extractor.ReflectExtract
import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.Representation
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
      |  case JsonArray(vs) => vs map write               mkString("[", ", ", "]")
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
        implicitArguments = Seq()
      )
      val res = ReflectExtract(ctx)
      res should contain only (call, decl)
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
      val expected = CallSite(
        location = None,
        name =
          "_empty_.nested.a2c(Lnested/A;Lscala/Function1;Lscala/Function1;)Lnested/C;.",
        code = "<No Code Yet>",
        isSynthetic = true,
        typeArguments = Seq(),
        implicitArguments = Seq(
          CallSite(
            location = None,
            name = "_empty_.nested.a2b(Lnested/A;)Lnested/B;.",
            code = "<No Code Yet>",
            isSynthetic = true,
            typeArguments = Seq(),
            implicitArguments = Seq(),
            declaration = Declaration(
              name = "nested.a2b",
              kind = "def",
              location = None,
              isImplicit = true,
              signature = Some(
                Signature(
                  typeParams = Seq(),
                  parameterLists = Seq(
                    DeclaredParameterList(
                      isImplicit = false,
                      params = Seq(
                        DeclaredParameter(
                          name = "a",
                          tipe = Type("A")
                        ))
                    )),
                  returnType = Type("B")
                )
              )
            )
          ),
          CallSite(
            location = None,
            name = "_empty_.nested.b2c(Lnested/B;)Lnested/C;.",
            code = "<No Code Yet>",
            isSynthetic = true,
            typeArguments = Seq(),
            implicitArguments = Seq(),
            declaration = Declaration(
              name = "nested.b2c",
              kind = "def",
              location = None,
              isImplicit = true,
              signature = Some(
                Signature(
                  typeParams = Seq(),
                  parameterLists = Seq(
                    DeclaredParameterList(
                      isImplicit = false,
                      params = Seq(
                        DeclaredParameter(
                          name = "b",
                          tipe = Type("B")
                        ))
                    )),
                  returnType = Type("C")
                )
              )
            )
          )
        ),
        declaration = Declaration(
          name = "nested.a2c",
          kind = "def",
          location = None,
          isImplicit = true,
          signature = Some(
            Signature(
              typeParams = Seq(),
              parameterLists = Seq(
                DeclaredParameterList(
                  isImplicit = false,
                  params = Seq(
                    DeclaredParameter(
                      name = "a",
                      tipe = Type("A")
                    ))
                ),
                DeclaredParameterList(
                  isImplicit = true,
                  params = Seq(
                    DeclaredParameter(
                      name = "b",
                      tipe = Type(name = "Function1",
                                  parameters = Seq(Type("T1"), Type("R")))
                    ),
                    DeclaredParameter(
                      name = "c",
                      tipe = Type(name = "Function1",
                                  parameters = Seq(Type("T1"), Type("R")))
                    )
                  )
                )
              ),
              returnType = Type("C")
            )
          )
        )
      )
      val res = ReflectExtract(ctx)

      debugPrint(Seq(expected), res)

      res should contain only expected
      println("End")
    }
  )

  /**
    * A small method to show the similarities between the expected and actual results.
    * Its main use is to see where the similarities start to break down.
    * More sophisticated diff methods require the use of external dependencies
    * @param expected
    * @param result
    */
  def debugPrint(expected: Seq[Representation.TopLevelElem],
                 result: Seq[Representation.TopLevelElem]) = {
    import sext._
    println("Expected output:")
    println(expected.treeString)
    println("Actual output:")
    println(result.treeString)
  }
}

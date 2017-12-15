package cz.cvut.fit.prl.scalaimplicit.extraction

import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.Representation._
import cz.cvut.fit.prl.scalaimplicit.extractor.ReflectExtract
import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.PrettyPrinters.PrettyInstances._
import cz.cvut.fit.prl.scalaimplicit.extractor.contexts.PrettyPrinters.prettyPrint
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
      /*
      val expected = CallSite(
        location = None,
        name = "test.JsonWriter",
        code = "<No Code Yet>",
        isSynthetic = true,
        declaration = Declaration(
          location = None,
          name = "test.JsonWriter",
          kind = "class",
          isImplicit = true,
          signature = Some(
            Signature(
              typeParams = Seq(
                Type(
                  name = "test.T",
                  constraints = Some(": JsonConverter")
                )),
              parameterLists = Seq(
                DeclaredParameterList(
                  isImplicit = false,
                  params = Seq(
                    DeclaredParameter(
                      name = "",
                      tipe = Type("test.T")
                    ))
                ),
                DeclaredParameterList(
                  isImplicit = true,
                  params = Seq(
                    DeclaredParameter(
                      name = "evidence",
                      tipe = Type(
                        name = "test.T",
                        constraints = Some(": JsonConverter")
                      )
                    ))
                )
              ),
              returnType = Type(
                name = "JsonWriter",
                parameters = Seq(Type("test.T"))
              )
            ))
        ),
        typeArguments = Seq(
          Type(
            name = "Seq",
            parameters = Seq(Type("Student"))
          )),
        implicitArguments = Seq()
      )
       */
      // I'll leave this on because this is a good stress test for the
      // reflection resolution, it has tons of corner cases, so I want
      // this test to trigger the assertions whenever we make a breaking change
      val res = ReflectExtract(ctx)
      res should have size 7
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
        name = "nested.a2c",
        code = "<No Code Yet>",
        location = None,
        isSynthetic = true,
        typeArguments = Seq(),
        implicitArguments = Seq(
          CallSite(
            location = None,
            name = "nested.a2b",
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
                          tipe = Type("nested.A")
                        ))
                    )),
                  returnType = Some(Type("nested.B"))
                )
              )
            )
          ),
          CallSite(
            location = None,
            name = "nested.b2c",
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
                          tipe = Type("nested.B")
                        ))
                    )),
                  returnType = Some(Type("nested.C"))
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
                      tipe = Type("nested.A")
                    ))
                ),
                DeclaredParameterList(
                  isImplicit = true,
                  params = Seq(
                    DeclaredParameter(
                      name = "b",
                      tipe = Type(name = "scala.Function1",
                                  parameters = Seq(Type("scala.Function1.T1"),
                                                   Type("scala.Function1.R")))
                    ),
                    DeclaredParameter(
                      name = "c",
                      tipe = Type(name = "scala.Function1",
                                  parameters = Seq(Type("scala.Function1.T1"),
                                                   Type("scala.Function1.R")))
                    )
                  )
                )
              ),
              returnType = Some(Type("nested.C"))
            )
          )
        )
      )
      val res = ReflectExtract(ctx)

      //debugPrint(Seq(expected), res)

      res should contain only expected
      println("End")
    }
  )

  checkReflContext(
    "Class Conversion",
    """
      |object classConv {
      |trait UselessParent {}
      |trait Useless extends UselessParent {}
      | trait Writer[A] {
      |  def write(x: A): String
      | }
      | implicit object IntWriter extends Writer[Int] with Useless {
      |  def write(x: Int) = (x * 2).toString
      | }
      | implicit class Hello[T: Writer](s: T) { def hello(): String = implicitly[Writer[T]].write(s) }
      | println( 2.hello() )
      |}
    """.trim.stripMargin, { ctx =>
      val expected = CallSite(
        name = "classConv.Hello",
        code = "<No Code Yet>",
        location = None,
        isSynthetic = true,
        declaration = Declaration(
          name = "classConv.Hello",
          kind = "def",
          location = None,
          isImplicit = true,
          signature = Some(
            Signature(
              typeParams = Seq(Type("classConv.T")),
              parameterLists = Seq(
                DeclaredParameterList(
                  isImplicit = false,
                  params = Seq(DeclaredParameter("s", Type("classConv.T")))
                ),
                DeclaredParameterList(
                  isImplicit = true,
                  params = Seq(
                    DeclaredParameter(
                      name = "evidence$1",
                      tipe = Type(
                        name = "classConv.Writer",
                        parameters = Seq(Type("classConv.Writer.A"))
                      )
                    ))
                )
              ),
              returnType =
                Some(Type("classConv.Hello[T]", parameters = Seq(Type("T"))))
            )),
          parents = Seq()
        ),
        typeArguments = Seq(Type("scala.Int")),
        implicitArguments = Seq(
          CallSite(
            name = "classConv.IntWriter",
            code = "<No Code Yet>",
            location = None,
            isSynthetic = true,
            declaration = Declaration(
              name = "classConv.IntWriter",
              kind = "object",
              location = None,
              isImplicit = true,
              signature = Some(Signature()),
              parents = Seq(
                Parent(
                  name = "classConv.Useless",
                  declaration = Declaration(
                    name = "classConv.Useless",
                    kind = "abstract trait",
                    location = None,
                    isImplicit = false,
                    signature = Some(Signature())
                  ),
                  typeArguments = Seq()
                ),
                Parent(
                  name = "classConv.Writer",
                  declaration = Declaration(
                    name = "classConv.Writer",
                    kind = "abstract trait",
                    location = None,
                    isImplicit = false,
                    signature = Some(
                      Signature(
                        typeParams = Seq(Type("classConv.Writer.A"))
                      ))
                  ),
                  typeArguments = Seq(Type("scala.Int"))
                )
              )
            ),
            typeArguments = Seq(),
            implicitArguments = Seq()
          )
        )
      )
      val res = ReflectExtract(ctx)
      res should contain(expected)

      val resStrings =
        res.map(x => prettyPrint(x.asInstanceOf[CallSite])(PrettyCallSite))

      val expectedStrings = prettyPrint(expected)(PrettyCallSite)
      resStrings should contain(prettyPrint(expected)(PrettyCallSite))
      println("End")
    }
  )

  checkPrettyReflRes(
    "Class Conversion with pretty Printing",
    """
      |object classConvPretty {
      |trait UselessParent {}
      |trait Useless extends UselessParent {}
      | trait Writer[A] {
      |  def write(x: A): String
      | }
      | implicit object IntWriter extends Writer[Int] with Useless {
      |  def write(x: Int) = (x * 2).toString
      | }
      | implicit class Hello[T: Writer](s: T) { def hello(): String = implicitly[Writer[T]].write(s) }
      | println( 2.hello() )
      |}
    """.trim.stripMargin,
    Seq(
      """|[]:synthetic CallSite: classConvPretty.Hello[scala.Int]
        |[]:  Declaration: implicit def classConvPretty.Hello[classConvPretty.T](s: classConvPretty.T), (implicit evidence$1: classConvPretty.Writer[classConvPretty.Writer.A]): classConvPretty.Hello[T][T]
        |[]:  synthetic CallSite: classConvPretty.IntWriter[]
        |[]:    Declaration: implicit object classConvPretty.IntWriter extends (abstract trait classConvPretty.Useless, abstract trait classConvPretty.Writer[classConvPretty.Writer.A = scala.Int])
        """.trim.stripMargin,
      """[]:CallSite: scala.Predef.implicitly[classConvPretty.Writer[classConvPretty.Hello.T]]
        |[]:  Declaration: def scala.Predef.implicitly[scala.Predef.T](implicit e: scala.Predef.T): T
        |[]:  synthetic CallSite: classConvPretty.Hello.[]
        |[]:    Declaration: final package classConvPretty.Hello.""".trim.stripMargin
    )
  )

  /**
    * A small method to show the similarities between the expected and actual results.
    * Its main use is to see where the similarities start to break down.
    * More sophisticated diff methods require the use of external dependencies
    * @param expected
    * @param result
    */
  def debugPrint(result: Seq[Representation.TopLevelElem]) = {
    println(result.map(x => prettyPrint(x)))
  }
}

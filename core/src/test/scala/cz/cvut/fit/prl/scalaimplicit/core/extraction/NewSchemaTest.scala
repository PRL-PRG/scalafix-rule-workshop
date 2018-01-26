package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.{
  ExtractionResult,
  ReflectExtract
}
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters.PrettyInstances.PrettyCallSite
import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest

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
      val res = ReflectExtract(ctx).callSites
      res should have size 7
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
      val expected = ExtractionResult(
        Seq(
          CallSite(
            name = "nested.a2c",
            code =
              "nested.this.a2c(*)({\n  ((a: A) => nested.this.a2b(a))\n}, {\n  ((b: B) => nested.this.b2c(b))\n})",
            location = Some(Location("", 8, 15)),
            isSynthetic = true,
            typeArguments = Seq(),
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
                                      parameters =
                                        Seq(Type("scala.Function1.T1"),
                                            Type("scala.Function1.R")))
                        ),
                        DeclaredParameter(
                          name = "c",
                          tipe = Type(name = "scala.Function1",
                                      parameters =
                                        Seq(Type("scala.Function1.T1"),
                                            Type("scala.Function1.R")))
                        )
                      )
                    )
                  ),
                  returnType = Some(Type("nested.C"))
                )
              )
            ),
            implicitArguments = Seq(
              ImplicitArgument(
                name = "nested.a2b",
                code = "nested.this.a2b(a)",
                typeArguments = Seq(),
                arguments = Seq(
                  Argument(
                    code = "a"
                  )
                ),
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
              ImplicitArgument(
                name = "nested.b2c",
                code = "nested.this.b2c(b)",
                typeArguments = Seq(),
                arguments = Seq(
                  Argument(
                    code = "b"
                  )
                ),
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
            )
          )
        ),
        Set()
      )
      val res = ReflectExtract(ctx).normalized.onlyCallSites

      val jsondiff = compareJSON(res, expected)
      jsondiff shouldBe empty

      val resStrings: Seq[String] =
        res.callSites
          .map(x => prettyPrint(x)(PrettyCallSite))
      val expectedStrings: Seq[String] =
        Seq(prettyPrint(expected.callSites.head)(PrettyCallSite))

      val diff = compareContents(lines(resStrings), lines(expectedStrings))
      diff shouldBe empty

      res.normalizedCallSites should contain only (expected.callSites: _*)
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
        code = "classConv.this.Hello[Int](*)(classConv.this.IntWriter)",
        location = Some(Location("", 10, 11)),
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
          ImplicitArgument(
            name = "classConv.IntWriter",
            code = "classConv.this.IntWriter",
            declaration = Declaration(
              name = "classConv.IntWriter",
              kind = "final object",
              location = None,
              isImplicit = true,
              signature = Some(
                Signature(
                  returnType = Some(Type("classConv.IntWriter.type"))
                )),
              parents = Seq(
                Parent(
                  name = "classConv.Useless",
                  declaration = Declaration(
                    name = "classConv.Useless",
                    kind = "abstract trait",
                    location = None,
                    isImplicit = false,
                    signature = Some(
                      Signature(
                        returnType =
                          Some(Type("classConv.UselessParent {\n  \n}"))
                      ))
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
                        typeParams = Seq(Type("classConv.Writer.A")),
                        returnType = Some(Type(
                          "[A]scala.AnyRef {\n  def write(x: A): String\n}"))
                      ))
                  ),
                  typeArguments = Seq(Type("scala.Int"))
                )
              )
            ),
            typeArguments = Seq(),
            arguments = Seq()
          )
        )
      )
      val res = ReflectExtract(ctx).normalizedCallSites
        .filter(_.code.startsWith("classConv"))

      val resStrings: Seq[String] =
        res
          .map(x => prettyPrint(x)(PrettyCallSite))
      val expectedStrings: Seq[String] =
        Seq(prettyPrint(expected)(PrettyCallSite))

      val diff = compareContents(lines(resStrings), lines(expectedStrings))
      diff shouldBe empty
      res should contain only (expected)
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
      """|[:10:11]:scs: classConvPretty.Hello[scala.Int]
         |?:  implicit def classConvPretty.Hello[classConvPretty.T](s: classConvPretty.T), (implicit evidence$1: classConvPretty.Writer[classConvPretty.Writer.A]): classConvPretty.Hello[T][T]
         |  iarg: classConvPretty.IntWriter
         |?:    implicit final object classConvPretty.IntWriter: classConvPretty.IntWriter.type extends (abstract trait classConvPretty.Useless, abstract trait classConvPretty.Writer[classConvPretty.Writer.A = scala.Int])
         |""".trim.stripMargin,
      """[:9:84]:cs: scala.Predef.implicitly[classConvPretty.Writer[classConvPretty.Hello.T]]
        |?:  def scala.Predef.implicitly[scala.Predef.T](implicit e: scala.Predef.T): T
        |  iarg: classConvPretty.Hello.
        |?:    final object classConvPretty.Hello.: classConvPretty.Hello..type
        |""".trim.stripMargin
    )
  )

  checkReflContext(
    "Infix application corner case",
    """
      |object inAppl {
      | val a = Seq(1, 2, 3)
      | val b = Seq("a", "b", "c")
      | (a zip b)
      |}
    """.trim.stripMargin,
    ctx => {
      val res = ReflectExtract(ctx)
      res.callSites should have size 1
    }
  )

  checkReflContext(
    "Call to local def with implicits",
    """
      |package loCall
      |object r {
      | trait A[T] { def a(implicit v: T): String }
      | val aimpl = new A[Int] { def a(implicit v: Int): String = v.toString }
      | implicit val iv: Int = 45
      | aimpl.a
      |}
    """.trim.stripMargin,
    ctx => {
      val css = ReflectExtract(ctx)
    }
  )

  /**
    * A small method to show the similarities between the expected and actual results.
    * Its main use is to see where the similarities start to break down.
    * More sophisticated diff methods require the use of external dependencies
    * @param res
    */
  def debugPrint(res: ExtractionResult) = {
    println(res.callSites.map(x => prettyPrint(x)))
  }
}

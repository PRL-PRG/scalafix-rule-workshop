package cz.cvut.fit.prl.scalaimplicit.core.extraction

import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters.PrettyInstances.PrettyCallSite
import cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers.PrettyPrinters._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{FailFastReflectExtract, ImplicitAnalysisResult}
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import cz.cvut.fit.prl.scalaimplicit.schema._

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
              typeParameters = Seq(
                Type(
                  name = "test.T",
                  constraints = Some(": JsonConverter")
                )),
              parameterLists = Seq(
                ParameterList(
                  isImplicit = false,
                  params = Seq(
                    Parameter(
                      name = "",
                      parameterType = Type("test.T")
                    ))
                ),
                ParameterList(
                  isImplicit = true,
                  params = Seq(
                    Parameter(
                      name = "evidence",
                      parameterType = Type(
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
      val res = FailFastReflectExtract(ctx).callSites
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
      val expected = ImplicitAnalysisResult(
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
                  typeParameters = Seq(),
                  parameterLists = Seq(
                    ParameterList(
                      isImplicit = false,
                      parameters = Seq(
                        Parameter(
                          name = "a",
                          parameterType = Type("nested.A")
                        ))
                    ),
                    ParameterList(
                      isImplicit = true,
                      parameters = Seq(
                        Parameter(
                          name = "b",
                          parameterType = Type(name = "scala.Function1",
                                               parameters =
                                                 Seq(Type("scala.Function1.T1"),
                                                     Type("scala.Function1.R")))
                        ),
                        Parameter(
                          name = "c",
                          parameterType = Type(name = "scala.Function1",
                                               parameters =
                                                 Seq(Type("scala.Function1.T1"),
                                                     Type("scala.Function1.R")))
                        )
                      )
                    )
                  ),
                  returnType = Type("nested.C")
                )
              )
            ),
            implicitArguments = Seq(
              Argument(
                code = "nested.this.a2b(a)",
                info = Some(ArgumentInfo(
                  name = "nested.a2b",
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
                        typeParameters = Seq(),
                        parameterLists = Seq(
                          ParameterList(
                            isImplicit = false,
                            parameters = Seq(Parameter(
                              name = "a",
                              parameterType = Type("nested.A")
                            ))
                          )),
                        returnType = Type("nested.B")
                      )
                    )
                  )
                ))
              ),
              Argument(
                code = "nested.this.b2c(b)",
                info = Some(ArgumentInfo(
                  name = "nested.b2c",
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
                        typeParameters = Seq(),
                        parameterLists = Seq(
                          ParameterList(
                            isImplicit = false,
                            parameters = Seq(Parameter(
                              name = "b",
                              parameterType = Type("nested.B")
                            ))
                          )),
                        returnType = Type("nested.C")
                      )
                    )
                  )
                ))
              )
            )
          )
        ),
        Set()
      )
      val res = FailFastReflectExtract(ctx).normalized.onlyCallSites

      val resStrings: Seq[String] =
        res.callSites
          .map(x => prettyPrint(x)(PrettyCallSite))
      val expectedStrings: Seq[String] =
        Seq(prettyPrint(expected.callSites.head)(PrettyCallSite))

      val diff = compareContents(lines(resStrings), lines(expectedStrings))
      diff shouldBe empty

      val jsondiff = compareJSON(res, expected)
      jsondiff shouldBe empty
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
          kind = "class",
          location = None,
          isImplicit = true,
          signature = Some(
            Signature(
              typeParameters = Seq(Type("classConv.Hello.T")),
              parameterLists = Seq(
                ParameterList(
                  isImplicit = false,
                  parameters = Seq(Parameter("s", Type("classConv.Hello.T")))
                ),
                ParameterList(
                  isImplicit = true,
                  parameters = Seq(
                    Parameter(
                      name = "evidence$1",
                      parameterType = Type(
                        name = "classConv.Writer",
                        parameters = Seq(Type("classConv.Writer.A"))
                      )
                    ))
                )
              ),
              returnType =
                Type("classConv.Hello[T]", parameters = Seq(Type("T")))
            )),
          parents = Seq()
        ),
        typeArguments = Seq(Type("scala.Int")),
        implicitArguments = Seq(
          Argument(
            code = "classConv.this.IntWriter",
            info = Some(
              ArgumentInfo(
                name = "classConv.IntWriter",
                declaration = Declaration(
                  name = "classConv.IntWriter",
                  kind = "final object",
                  location = None,
                  isImplicit = true,
                  signature = Some(
                    Signature(
                      returnType = Type("classConv.IntWriter.type")
                    )),
                  parents = Seq(
                    Parent(
                      name = "classConv.Useless",
                      declaration = Declaration(
                        name = "classConv.Useless",
                        kind = "abstract trait",
                        location = None,
                        isImplicit = false,
                        signature = Some(Signature(
                          returnType = Type("<notype>")
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
                            typeParameters = Seq(Type("classConv.Writer.A")),
                            returnType = Type("<notype>")
                          ))
                      ),
                      typeArguments = Seq(Type("scala.Int"))
                    )
                  )
                ),
                typeArguments = Seq(),
                arguments = Seq()
              ))
          )
        )
      )
      val res = FailFastReflectExtract(ctx).normalizedCallSites
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
         |?:  implicit class classConvPretty.Hello[classConvPretty.Hello.T](s: classConvPretty.Hello.T), (implicit evidence$1: classConvPretty.Writer[classConvPretty.Writer.A]): classConvPretty.Hello[T][T]
         |  iarg: classConvPretty.IntWriter
         |?:    implicit final object classConvPretty.IntWriter: classConvPretty.IntWriter.type extends (abstract trait classConvPretty.Useless, abstract trait classConvPretty.Writer[classConvPretty.Writer.A = scala.Int])
         |""".trim.stripMargin,
      """[:9:84]:cs: scala.Predef.implicitly[classConvPretty.Writer[classConvPretty.Hello.T]]
        |?:  def scala.Predef.implicitly[scala.Predef.T](implicit e: scala.Predef.T): T
        |  iarg: classConvPretty.Hello.evidence$1
        |?:    implicit val classConvPretty.Hello.evidence$1: classConvPretty.Writer[T][T]
        |""".trim.stripMargin
    )
  )

  checkReflContext(
    "EV is just code",
    """
      |package playground
      |object constructors {
      |case class M()
      | implicit def a(s: String): M = ???
      | class ExtendedAny[A](what: A)(implicit sth: A => M) {}
      | def Create[T](x: T)(implicit ev: T => M) = new ExtendedAny[T](x)
      | Create("Hello")
      |}
    """.trim.stripMargin,
    ctx => {
      val res = FailFastReflectExtract(ctx)
      res.callSites.forall(_.implicitArguments.forall(_.info.isDefined)) shouldBe true
      println(res)
    }
  )

  /*
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
   */

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
      val css = FailFastReflectExtract(ctx)
    }
  )

  /**
    * A small method to show the similarities between the expected and actual results.
    * Its main use is to see where the similarities start to break down.
    * More sophisticated diff methods require the use of external dependencies
    * @param res
    */
  def debugPrint(res: ImplicitAnalysisResult) = {
    println(res.callSites.map(x => prettyPrint(x)))
  }
}

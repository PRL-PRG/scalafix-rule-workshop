package cz.cvut.fit.prl.scalaimplicit.query

import cz.cvut.fit.prl.scalaimplicit.schema._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{FailFastReflectExtract, ImplicitAnalysisResult}
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import cz.cvut.fit.prl.scalaimplicit.matcher._

class QueriesTest extends SemanticdbTest with Matchers with SchemaMatchers {

  //  val conversionFunction: CallSite => Boolean = {
  //    case CallSite(
  //    _,
  //    _,
  //    _,
  //    true,
  //    Declaration(
  //    _,
  //    kind,
  //    _,
  //    true,
  //    Some(
  //    Signature(_,
  //    Seq(DeclaredParameterList(Seq(parameter), false)),
  //    _)),
  //    _),
  //    _,
  //    _) if (kind.contains("def") || kind.contains("class")) =>
  //      true
  //    case _ => false
  //  }
  //
  //  val filename = "/home/krikava/Research/Projects/scala-implicits/runs/./projects/Metascala/_reports/results-callsites.json"
  //  val filename2 = "/home/krikava/Research/Projects/scala-implicits/runs/./projects/elasticmq/_reports/results-callsites.json"
  //  //val manifest = ProjectReport.loadReportsFromManifest("../runs/manifest.json.1")
  //  //val Metascala = manifest(0)
  //
  //  test("first test") {
  //    import io.circe.generic.auto._
  //
  //    val m: Matcher[CallSite] =
  //      and(
  //        isSynthetic,
  //        declaration(
  //          isImplicit,
  //          kind(in("def", "class")),
  //          signature(
  //            parameterLists(
  //              size(1),
  //              contains(
  //                !isImplicit,
  //                parameters(size(1))
  //              )
  //            )
  //          )
  //        )
  //      )
  //
  //    println(m.description)
  //
  //    val r = JsonQuery.query(filename, m)
  //    println(r.size)
  //
  //    val r2 = JsonQuery.queryAll(Seq(filename, filename2), m)
  //    println(r2.size)
  //  }
  //
  //  test("second test") {
  //    import io.circe.generic.auto._
  //
  //    val m: Matcher[CallSite] =
  //      and(
  //        isSynthetic,
  //        declaration(
  //          isImplicit,
  //          kind(in("def".r, "class".r)),
  //          signature(
  //            parameterLists(
  //              or(
  //                inOrderOnly(
  //                  and(!isImplicit, parameters(size(1)))
  //                ),
  //                inOrderOnly(
  //                  parameters(size(1)),
  //                  isImplicit
  //                )
  //              )
  //            )
  //          )
  //        )
  //      )
  //
  //    println(m.description)
  //
  //    val r = JsonQuery.query(filename, m)
  //    println(r.size)
  //
  //    val r2 = JsonQuery.queryAll(Seq(filename, filename2), m)
  //    println(r2.size)
  //  }

  //      val cs = Metascala.result.callSites
  //
  //      val q =
  //        isSynthetic &&
  //        declaration(
  //          isImplicit &&
  //            kind(in("def".r, "class".r)) &&
  //            signature(
  //              contains(
  //                parameterLists(
  //                  inOrder(!isImplicit && size(is(1))) ||
  //                  inOrder(!isImplicit && size(is(1)), isImplicit && size(is(1)))
  //                )
  //              )
  //            )
  //        )
  //
  //      val IC = cs.query(q)
  //
  //      val matched1 = IC.filter(_.matches).size
  //      val matched2 = cs.filter(conversionFunction).size
  //
  //      println((matched1, matched2))
  //
  //      val X = cs.map(x => (conversionFunction(x), x.matches(q)))
  //      val missed = X collect {
  //        case (true, m : Mismatch[_]) => m.reason
  //      }
  //      val reasons = missed.groupBy(x => x).map { case (k, v) => k -> v.size}
  //
  //      println(reasons)
  //    }

  //  checkReflContext(
  //    "Type params?",
  //    """
  //      |object o5 {
  //      |   implicit val foo = "Foo"
  //      |   def a[A](b: A)(implicit f: A) = ???
  //      |   a("Hello")
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //      println(res.callSites)
  //    }
  //  )
  //
  //  checkReflContext(
  //    "my implicit call",
  //    """
  //      | object o {
  //      |   trait Jsonable[A] {
  //      |     def toJson(x: A): String
  //      |   }
  //      |
  //      |   case class X(x: Int)
  //      |
  //      |   implicit val XJsonable = new Jsonable[X] {
  //      |     def toJson(x: X) = s"{x: ${x.x}}"
  //      |   }
  //      |
  //      |   implicit class AnyJsonable[A: Jsonable](that: A) {
  //      |     def toJson: String = implicitly[Jsonable[A]].toJson(that)
  //      |   }
  //      |
  //      |   X(1).toJson
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx).normalized
  //
  //      import Util._
  //      println(res.callSites.toPrettyString)
  //    })
  //
  //  checkReflContext(
  //    "Multiple applies",
  //    """
  //      |object o2 {
  //      |  class X1
  //      |  class Y1 {
  //      |    def f: String = ???
  //      |  }
  //      |  class A
  //      |  object A {
  //      |    def apply[T <: X1](a: T): A = ???
  //      |    implicit def apply[T <: {def f: String}](a: T)(implicit b: Y1): A = ???
  //      |  }
  //      |
  //      |  implicit val y1 = new Y1
  //      |  val mya: A = new Y1
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //
  //      println(res.callSites)
  //      println("-----")
  //      println(res.declarations)
  //    }
  //  )

  //  checkReflContext(
  //    "Multiple applies",
  //    """
  //      |object o2 {
  //      |  class A
  //      |  object A {
  //      |    def apply(a: Int): A = ???
  //      |    implicit def apply(a: String): A = ???
  //      |  }
  //      |
  //      |  case class B(a: A)
  //      |
  //      |  B("xzy")
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //
  //      println(res.callSites)
  //      println("-----")
  //      println(res.declarations)
  //    }
  //  )

  //  checkReflContext(
  //    "Multiple applies",
  //    """
  //      |object o {
  //      |trait Printable[T] {
  //      |  def print(x: T): String
  //      |}
  //      |def print[T](x: T)(implicit ev: Printable[T]) = ev.print(x)
  //      |
  //      |case class Shape(n: Int)
  //      |implicit object ShapePrintable extends Printable[Shape] {
  //      |  def print(x: Shape) = x.n match {
  //      |    case 3 => "a triangle"
  //      |    case 4 => "a square"
  //      |    case n => s"a shape with $n sides"
  //      |  }
  //      |}
  //      |print(Shape(5)) // "a shape with 5 sides"
  //      |
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //
  //      println(res.callSites)
  //      val x = res.callSites(0).matches(declaration(kind(is("class")), signature(parameterLists(contains(and(isImplicit, parameters(size(1))))))))
  //      println(x.reason)
  //      println("-----")
  //      println(res.declarations)
  //    }
  //  )

  //  checkReflContext(
  //    "Multiple applies",
  //    """
  //      |object o {
  //      |class A
  //      |class B
  //      |class C
  //      |class D
  //      |
  //      |implicit def a2b(implicit x: A): B = new B
  //      |implicit def b2c(implicit x: B): C = new C
  //      |implicit def c2d(implicit x: C): D = new D
  //      |
  //      |def x(implicit d: D) = 0
  //      |
  //      |implicit val a = new A
  //      |
  //      |x
  //      |
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //
  //      println(res.callSites)
  //      println("-----")
  //      println(res.declarations)
  //    }
  //  )

  checkReflContext(
    "Multiple applies",
    """
      |object o {
      |
      |  class Z
      |  class Y extends Z
      |
      |  def x[A](x: A)(implicit ev: A =:= String) = x.isEmpty
      |  def y[A](x: A)(implicit ev: A <:< String) = x.isEmpty
      |
      |  x("A")
      |  y("B")
      |
      |}
    """.stripMargin,
    { ctx =>
      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)

      println(res.callSites)
      println("-----")
      println(res.declarations)
    }
  )
  // checkReflContext(
  //    "Multiple applies",
  //    """
  //      |object o {
  //      |
  //      |  val x: Seq[(Int, Int)] = ???
  //      |
  //      |  x.map((_, 1))
  //      |
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //
  //      println(res.callSites)
  //      println("-----")
  //      println(res.declarations)
  //    }
  //  )
  // checkReflContext(
  //    "Multiple applies",
  //    """
  //      |object o {
  //      |
  //      |  val x: Int = {
  //      |
  //      |    implicit val y: String = ""
  //      |
  //      |    def f(implicit e: String) = ???
  //      |
  //      |    f
  //      |
  //      |  }
  //      |
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //
  //      println(res.callSites)
  //      println("-----")
  //      println(res.declarations)
  //    }
  //  )


  //  checkReflContext(
  //    "Multiple applies2",
  //    """
  //      |object o2 {
  //      |  implicit class A(s: String) {
  //      |   def hash = ???
  //      |  }
  //      |  "Hello".hash
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //      println(res.callSites)
  //    }
  //  )
  //
  //  checkReflContext(
  //    "Multiple applies3",
  //    """
  //      |object o2 {
  //      |  class X {
  //      |    implicit class A(s: String) {
  //      |      def hash = ???
  //      |    }
  //      |  }
  //      |  val x = new X
  //      |  import x._
  //      |  "Hello".hash
  //      |}
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx)
  //      println(res.callSites)
  //    }
  //  )

  //
  //    checkReflContext(
  //      "my implicit call2",
  //      """
  //        | object o {
  //        |   trait X
  //        |   trait Y extends X
  //        |   implicit class Z(x: Int) extends Y {
  //        |     def a = 1
  //        |   }
  //        |
  //        |   1.a
  //        | }
  //      """.stripMargin,
  //      { ctx =>
  //        val res = FailFastReflectExtract(ctx).normalized
  //
  //        println(res.callSites)
  //
  //      })

  //  checkReflContext(
  //    "Recursive implicit view bounds",
  //    """
  //      |object tt {
  //      |  case class View[A](a: A) {
  //      |    def view: Unit = s"View of $a"
  //      |  }
  //      |
  //      |  case class C()
  //      |  case class D()
  //      |
  //      |  implicit def any2view[A](x: A): View[A] = View(x)
  //      |
  //      |  def f[A <% View[A]](a: Seq[A]): Seq[A] = a flatMap (x => {x.view; f(a)})
  //      |  def g[A](a: Seq[A])(implicit ev: A => View[A]) = ???
  //      |
  //      |  f(Seq(C(), D()))
  //      |  //g(Seq(C(), D()))
  //      |}
  //    """.stripMargin,
  //    ctx => {
  //      import Util._
  //      val a = FailFastReflectExtract(ctx)
  //      println(a.callSites.toPrettyString)
  //    }
  //  )
  //
  //  checkReflContext(
  //    "Recursive implicit view bounds2",
  //    """
  //      |object tt {
  //      |  case class Number[A](a: A) {
  //      |    def *(b: Seq[A]): A = ???
  //      |  }
  //      |  implicit def d[A](p: A): Number[A] = Number[A](p)
  //      |  def b[A](a: Seq[A])(implicit ev: A => Number[A]): Seq[A] =
  //      |    a map (x => x * b(Seq(x)))
  //      |  b(Seq(2.0))
  //      |}
  //    """.stripMargin,
  //    ctx => {
  //      import Util._
  //      val a = FailFastReflectExtract(ctx)
  //      println(a.callSites.toPrettyString)
  //    }
  //  )
}

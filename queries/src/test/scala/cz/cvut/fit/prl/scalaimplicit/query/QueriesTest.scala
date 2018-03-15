package cz.cvut.fit.prl.scalaimplicit.query

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{FailFastReflectExtract, ImplicitAnalysisResult}
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import cz.cvut.fit.prl.scalaimplicit.matcher._

class QueriesTest extends SemanticdbTest with Matchers with SchemaMatchers {

  val conversionFunction: CallSite => Boolean = {
    case CallSite(
    _,
    _,
    _,
    true,
    Declaration(
    _,
    kind,
    _,
    true,
    Some(
    Signature(_,
    Seq(ParameterList(Seq(parameter), false)),
    _)),
    _),
    _,
    _) if (kind.contains("def") || kind.contains("class")) =>
      true
    case _ => false
  }

  val filename = "/home/krikava/Research/Projects/scala-implicits/runs/./projects/Metascala/_reports/results-callsites.json"
  val filename2 = "/home/krikava/Research/Projects/scala-implicits/runs/./projects/elasticmq/_reports/results-callsites.json"
  //val manifest = ProjectReport.loadReportsFromManifest("../runs/manifest.json.1")
  //val Metascala = manifest(0)

  test("first test") {
    import io.circe.generic.auto._

    val m: Matcher[CallSite] =
      and(
        isSynthetic,
        declaration(
          isImplicit,
          kind(in("def", "class")),
          signature(
            parameterLists(
              size(1),
              contains(
                !isImplicit,
                parameters(size(1))
              )
            )
          )
        )
      )

    println(m.description)

    val r = JsonQuery.query(filename, m)
    println(r.size)

    val r2 = JsonQuery.queryAll(Seq(filename, filename2), m)
    println(r2.size)
  }

  test("second test") {
    import io.circe.generic.auto._

    val m: Matcher[CallSite] =
      and(
        isSynthetic,
        declaration(
          isImplicit,
          kind(in("def".r, "class".r)),
          signature(
            parameterLists(
              or(
                inOrderOnly(
                  and(!isImplicit, parameters(size(1)))
                ),
                inOrderOnly(
                  parameters(size(1)),
                  isImplicit
                )
              )
            )
          )
        )
      )

    println(m.description)

    val r = JsonQuery.query(filename, m)
    println(r.size)

    val r2 = JsonQuery.queryAll(Seq(filename, filename2), m)
    println(r2.size)
  }

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

  checkReflContext(
    "my implicit call",
    """
      | object o {
      |   class A
      |   implicit class M(that: Int) { def value = new A }
      |   2.value
      | }
    """.stripMargin,
    { ctx =>
      val res: ImplicitAnalysisResult = FailFastReflectExtract(ctx).normalized

      import Util._
      println(res.callSites(0).toPrettyString)
      println("")
      println(res.declarations.toPrettyString)
      //
      println("")
      //      val m = res.callSites.query(
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
      //      val n = res.callSites.filter(conversionFunction)
      //
      //      println(m)
    })
  //
  //  checkReflContext(
  //    "my implicit call2",
  //    """
  //      | object o {
  //      |   implicit def M(that: Int, that2: Int): Int = 1
  //      |
  //      |   implicitly[(Int, Int) => Int]
  //      | }
  //    """.stripMargin,
  //    { ctx =>
  //      val res: ExtractionResult = FailFastReflectExtract(ctx).normalized
  //
  //      val m = res.callSites.query(
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
  //      val n = res.callSites.filter(conversionFunction)
  //
  //      println(m(0).reason)
  //    })

}

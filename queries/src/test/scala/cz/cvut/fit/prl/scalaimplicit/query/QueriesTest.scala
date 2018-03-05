package cz.cvut.fit.prl.scalaimplicit.query

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation._
import cz.cvut.fit.prl.scalaimplicit.core.extractor.{ExtractionResult, FailFastReflectExtract}
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import cz.cvut.fit.prl.scalaimplicit.core.reports.ProjectReport
import cz.cvut.fit.prl.scalaimplicit.matcher.{ImplicitMatchers, Matchers, SchemaMatchers}

class QueriesTest extends SemanticdbTest with Matchers with ImplicitMatchers with SchemaMatchers {

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
    Seq(DeclaredParameterList(Seq(parameter), false)),
    _)),
    _),
    _,
    _) if (kind.contains("def") || kind.contains("class")) =>
      true
    case _ => false
  }

  val manifest = ProjectReport.loadReportsFromManifest("../runs/manifest.json.1")
  val Metascala = manifest(0)

  //  test("first test") {
  //    val cs = Metascala.result.callSites
  //
  //    val q =
  //      isSynthetic &&
  //      declaration(
  //        isImplicit &&
  //          kind(in("def".r, "class".r)) &&
  //          signature(
  //            contains(
  //              parameterLists(
  //                inOrder(!isImplicit && size(is(1))) ||
  //                inOrder(!isImplicit && size(is(1)), isImplicit && size(is(1)))
  //              )
  //            )
  //          )
  //      )
  //
  //    val IC = cs.query(q)
  //
  //    val matched1 = IC.filter(_.matches).size
  //    val matched2 = cs.filter(conversionFunction).size
  //
  //    println((matched1, matched2))
  //
  //    val X = cs.map(x => (conversionFunction(x), x.matches(q)))
  //    val missed = X collect {
  //      case (true, m : Mismatch[_]) => m.reason
  //    }
  //    val reasons = missed.groupBy(x => x).map { case (k, v) => k -> v.size}
  //
  //    println(reasons)
  //
  //
  //
  //  }

  checkReflContext(
    "my implicit call",
    """
      | object o {
      |   implicit class M(that: Int) { def value = that }
      |   2.value
      | }
    """.stripMargin,
    { ctx =>
      val res: ExtractionResult = FailFastReflectExtract(ctx).normalized

      val m = res.callSites.query(
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

      val n = res.callSites.filter(conversionFunction)

      println(m)
    })

  checkReflContext(
    "my implicit call2",
    """
      | object o {
      |   implicit def M(that: Int, that2: Int): Int = 1
      |
      |   implicitly[(Int, Int) => Int]
      | }
    """.stripMargin,
    { ctx =>
      val res: ExtractionResult = FailFastReflectExtract(ctx).normalized

      val m = res.callSites.query(
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

      val n = res.callSites.filter(conversionFunction)

      println(m(0).reason)
    })

}

package cz.cvut.fit.prl.scalaimplicit.core.heuristics

import cz.cvut.fit.prl.scalaimplicit.core.extractor.ExtractImplicits
import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.SemanticCtx
import cz.cvut.fit.prl.scalaimplicit.core.framework.SemanticdbTest
import org.langmeta.semanticdb.{ResolvedName, Synthetic}

class ImplicitClassExtensionTest extends SemanticdbTest {

  /**
    * Define a method to identify the definition of the
    * Implicit class extension pattern.
    *
    * This can be done after extraction in an R script since, as it is shown,
    * we already have the necessary data.
    * @param ctx
    */
  def checkDefinition(ctx: SemanticCtx): Unit = {
    val res = ExtractImplicits(ctx)
    res.implicits should have size 1
    val classDeclaration = res.implicits.head
    classDeclaration.kind shouldEqual "class"
  }

  /**
    * Define some conditions that all usages of this pattern must use.
    * These should be sufficient conditions to determine that a synthetic wrapper
    * corresponds to this pattern's usage.
    *
    * Note that there is some string parsing involved, which I think should be done
    * after the data is in the database.
    *
    * Note also that we need to capture a sort of Synthetic that we didn't before,
    * which will likely involve an extension of the model and the db schema.
    *
    * @param ctx
    */
  def checkUsage(ctx: SemanticCtx): Unit = {
    // See that the inserted call matches the expected fqn.of.constructor(*) pattern
    val usages =
      ctx.index.synthetics.filter(_.text.matches("""(\.?[\[\w\]]*)+\(\*\)"""))
    usages should not be empty
    usages.foreach { usage =>
      // Check that the return type is similar to the name of the class
      val symbol = usage.names.find(_.symbol.toString != "_star_.").get
      val parts = symbol.symbol.toString.split("""(?<=.*)(\(|\))""")
      val fqfn = parts(0)
      val fqreturn = parts(2).replace(";.", "").replace("/", ".").substring(1)
      fqfn should endWith(fqreturn)
    }
  }

  def namesInSamePlace(ctx: SemanticCtx,
                       usage: Synthetic): Iterable[ResolvedName] = {
    ctx.names.filter(
      x =>
        x.position.start == usage.position.start ||
          x.position.end == usage.position.end)
  }

  checkContext(
    "Symbols inside wrappers are still in the index",
    """
      |package a
      |object Definition {
      | implicit class Hello(s: String) { def hello = "Hello" + s }
      |}
      |object Usage {
      | import Definition._
      | val s: String = "World"
      | println("World".hello) /* 1 */
      | println(s.hello) /* 2 */
      |}
      |// From the repo wiki: https://github.com/PRL-PRG/scalafix-rule-workshop/wiki/Patterns:-Implicit-Class-Extension
    """.trim.stripMargin, { ctx =>
      checkDefinition(ctx)
      checkUsage(ctx)

      val usages = ctx.index.synthetics
        .filter(_.text.matches("""(\.?[\[\w\]]*)+\(\*\)"""))
      // (1) Check that there are no names in the same place as the constant conversion
      val constantUsage = usages.find(_.position.start == 163).get
      namesInSamePlace(ctx, constantUsage) shouldBe empty

      // (2) Check that there are some names in the same place as the variable conversion
      val variableUsage = usages.find(_.position.start == 195).get
      namesInSamePlace(ctx, variableUsage) should not be empty
    }
  )

  checkContext(
    "Example of class extension",
    """
      |package b
      |// Definition
      |object Definition {
      |  implicit class IntWithTimes(x: Int) {
      |    def times[A](f: => A): Unit = {
      |      def loop(current: Int): Unit =
      |        if(current > 0) {
      |          f
      |          loop(current - 1)
      |        }
      |      loop(x)
      |    }
      |  }
      |}
      |object Usage {
      | import Definition._
      | 5 times println("HI")
      |}
      |//From the official documentation: https://docs.scala-lang.org/overviews/cz.cvut.fit.prl.scalaimplicit.core/implicit-classes.html
    """.trim.stripMargin, { ctx =>
      checkDefinition(ctx)
      checkUsage(ctx)
    }
  )

  checkContext(
    "Tuple extension",
    """
      |package c
      |object Definition {
      | implicit class Tupple2Add(t: (Long, Long)) {
      |    def +(p: (Long, Long)) = (p._1 + t._1, p._2 + t._2)
      |  }
      |}
      |object Usage {
      | import Definition._
      | (2L, 4L) + (3L, 6L)
      |}
      |// From aerosolve: com/airbnb/aerosolve/training/pipeline/PipelineTestingUtil.scala
    """.trim.stripMargin, { ctx =>
      checkDefinition(ctx)
      checkUsage(ctx)
    }
  )

}

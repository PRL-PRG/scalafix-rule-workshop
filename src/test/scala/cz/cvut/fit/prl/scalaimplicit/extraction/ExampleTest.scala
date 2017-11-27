package cz.cvut.fit.prl.scalaimplicit.extraction

import cz.cvut.fit.prl.scalaimplicit.framework.SemanticdbTest

class ExampleTest extends SemanticdbTest {

  checkExtraction(
    "Example of a code snippet test",
    """
      |object ForCompImplicits {
      |  import scala.concurrent.ExecutionContext.Implicits.global
      |  for {
      |    a <- scala.concurrent.Future.successful(1)
      |    b <- scala.concurrent.Future.successful(2)
      |    if a < b
      |  } yield a
      |}
      """.trim.stripMargin, { result =>
      result.funs.nonEmpty shouldBe true

    }
  )

}

class ExampleTest extends SemanticdbTest {

  checkExtraction("""
      |object ForCompImplicits {
      |  import scala.concurrent.ExecutionContext.Implicits.global
      |  for {
      |    a <- scala.concurrent.Future.successful(1)
      |    b <- scala.concurrent.Future.successful(2)
      |    if a < b
      |  } yield a
      |}
      """.trim.stripMargin, { result =>

    result.funs shouldBe empty

  })

}

package implicits

import scala.concurrent.{ExecutionContext, Future}

class Futures_Test {

  // import ExecutionContext.Implicits.global
  implicit val ec = ExecutionContext.global

  val f1 = Future {
    1+1
  }

  val f2 = Future {
    2+2
  }

  for {
    v1 <- f1
    v2 <- f2
  } yield v1+v2

  case class Input(x: Int) {
    def +(other: Input) = Input(x + other.x)
    def +(other: Int) = Input(x + other)
    1 + 2
    List(x).map(num => Input(num + 1) + Input(2) + 3)
  }

}
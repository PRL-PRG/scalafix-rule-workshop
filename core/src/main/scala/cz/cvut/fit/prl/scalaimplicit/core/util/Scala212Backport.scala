package cz.cvut.fit.prl.scalaimplicit.core.util

trait Scala212Backport {

  implicit class Either212[A, B](that: Either[A, B]) {
    def flatMap[A1 >: A, B1](f: B => Either[A1, B1]): Either[A1, B1] = that match {
      case Right(b) => f(b)
      case _ => this.asInstanceOf[Either[A1, B1]]
    }

    def map[B1](f: B => B1): Either[A, B1] = that match {
      case Right(b) => Right(f(b))
      case _ => this.asInstanceOf[Either[A, B1]]
    }
  }

}

object Scala212Backport extends Scala212Backport
package cz.cvut.fit.prl.scalaimplicit.queries

private[queries] trait OverloadHacks {

  trait OverloadHack1

  trait OverloadHack2

  implicit val overloadHack1 = new OverloadHack1 {}
  implicit val overloadHack2 = new OverloadHack2 {}
}

private[queries] object OverloadHacks extends OverloadHacks

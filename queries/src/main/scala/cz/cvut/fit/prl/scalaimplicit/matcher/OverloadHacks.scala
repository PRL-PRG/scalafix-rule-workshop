package cz.cvut.fit.prl.scalaimplicit.matcher

private[matcher] trait OverloadHacks {

  trait OverloadHack1

  trait OverloadHack2

  implicit val overloadHack1 = new OverloadHack1 {}
  implicit val overloadHack2 = new OverloadHack2 {}
}

private[matcher] object OverloadHacks extends OverloadHacks

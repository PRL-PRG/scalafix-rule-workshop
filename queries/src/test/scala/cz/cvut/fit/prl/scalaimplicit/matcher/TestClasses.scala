package cz.cvut.fit.prl.scalaimplicit.matcher

object TestClasses {

  class W

  class X(val x: Int) extends W {
    override def toString: String = s"X($x)"
  }

  class Y(x: Int, val y: Int) extends X(x) {
    override def toString: String = s"Y($x,$y)"
  }

  val w1 = new W
  val x1 = new X(1)
  val y1 = new Y(1, 1)

  val lw: List[W] = w1 :: x1 :: y1 :: Nil
  val lx: List[X] = x1 :: y1 :: Nil
  val ly: List[Y] = y1 :: Nil
}
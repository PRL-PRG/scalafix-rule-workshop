package cz.cvut.fit.prl.scalaimplicit.matcher


trait Matchers extends EqMatchers
  with IterableMatchers
  with OrderingMatchers
  with StringMatchers
  with LogicalMatchers
  with ImplicitMatchers


object Matchers extends Matchers
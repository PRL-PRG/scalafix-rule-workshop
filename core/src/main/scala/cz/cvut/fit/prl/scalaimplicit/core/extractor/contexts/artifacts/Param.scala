package cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.artifacts

import scala.meta.Position

// Common interface for all parameters (implicit and non-implicit)
// that can be passed to function applications
trait Param {
  def code: String
  def pos: Position
}

// Represents non-named parameters of synthetic function applications such as
// Hello(*)("Hihi") -> "Hihi" does not have a symbol
case class RawCode(code: String, pos: Position) extends Param

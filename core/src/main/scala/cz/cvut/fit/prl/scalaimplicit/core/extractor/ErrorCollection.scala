package cz.cvut.fit.prl.scalaimplicit.core.extractor

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.contexts.Representation.CallSite

class DebugLogger[A] {
  private var _queue: List[A] = List()
  def report(e: A) = _queue = e :: _queue
  def toFile(file: String) =
    Files.write(Paths.get(file),
                _queue
                  .map(_.toString)
                  .mkString("\n--------------------------------------\n")
                  .getBytes())
}

object ErrorCollection extends (() => DebugLogger[Throwable]) {
  val _coll = new DebugLogger[Throwable]()
  def apply() = _coll
}

object OrphanCallSites extends (() => DebugLogger[String]) {
  val _coll = new DebugLogger[String]
  def apply() = _coll
}

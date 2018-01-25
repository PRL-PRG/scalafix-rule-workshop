package cz.cvut.fit.prl.scalaimplicit.core.extractor

import java.nio.file.{Files, Paths}

import cz.cvut.fit.prl.scalaimplicit.core.extractor.representation.Representation.CallSite

class DebugLogger[A] {
  private var _queue: List[(String, A)] = List()
  def report(header: String, e: A) = _queue = (header, e) :: _queue
  def toFile(file: String) =
    Files.write(Paths.get(file),
                _queue
                  .map(x => s"${x._1}----------------\n${x._2.toString}")
                  .mkString("\n")
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

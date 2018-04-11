package cz.cvut.fit.prl.scalaimplicit.core.extractor

import java.io.{PrintStream, PrintWriter, StringWriter}
import java.nio.file.{Files, Paths}

/**
  * Class that buffers logs, and can print them to a file.
  * It does have state, but the only operation it allows is to add,
  * and not even toFile modifies the queue.
  * @tparam A
  */
class DebugLogger[A](prettyPrint: A => String) {
  private var _queue: List[(String, A)] = List()
  def report(header: String, e: A) = _queue = (header, e) :: _queue
  def toFile(file: String) =
    Files.write(Paths.get(file),
                _queue
                  .map(x => s"${x._1}----------------\n${prettyPrint(x._2)}")
                  .mkString("\n")
                  .getBytes())
}

// Global singleton instances of the logger.
object ErrorCollection extends (() => DebugLogger[Throwable]) {
  val _coll = new DebugLogger[Throwable](x => {
    val sw = new StringWriter
    x.printStackTrace(new PrintWriter(sw))
    sw.toString
  })
  def apply() = _coll
}

object OrphanCallSites extends (() => DebugLogger[String]) {
  val _coll = new DebugLogger[String](x => x.toString)
  def apply() = _coll
}

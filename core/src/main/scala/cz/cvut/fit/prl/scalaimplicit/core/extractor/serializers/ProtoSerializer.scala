package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import java.io.{FileInputStream, FileOutputStream, InputStream}

import scala.util.Try
import scalapb.GeneratedMessageCompanion

object ProtoSerializer {
  type Message[A] = scalapb.GeneratedMessage with scalapb.Message[A]

  def load[A <: Message[A] : GeneratedMessageCompanion](file: String): Try[Seq[A]] = {
    val companion = implicitly[GeneratedMessageCompanion[A]]
    val tin: Try[FileInputStream] = Try(new FileInputStream(file))

    try {
      for (in <- tin; xs <- Try(companion.streamFromDelimitedInput(in).toList)) yield xs
    } finally {
      tin.map(_.close())
    }
  }

  def loadStream[A <: Message[A] : GeneratedMessageCompanion](stream: InputStream): Try[Seq[A]] = {
    val companion = implicitly[GeneratedMessageCompanion[A]]
    try {
      Try(companion.streamFromDelimitedInput(stream).toList)
    } finally {
      stream.close()
    }
  }

  def save[A <: Message[A]](messages: Seq[A], file: String): Try[Unit] = {
    val tout = Try(new FileOutputStream(file))
    try {
      for (out <- tout; xs <- Try(messages.foreach(x => x.writeDelimitedTo(out)))) yield xs
    } finally {
      tout.map(_.close())
    }
  }
}

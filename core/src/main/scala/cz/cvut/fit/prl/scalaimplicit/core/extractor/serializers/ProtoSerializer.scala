package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import java.io.{FileInputStream, FileOutputStream}

import com.trueaccord.scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}

import scala.util.Try

object ProtoSerializer {
  type Msg[A] = GeneratedMessage with Message[A]

  def load[A <: Msg[A] : GeneratedMessageCompanion](file: String): Try[Seq[A]] = {
    val companion = implicitly[GeneratedMessageCompanion[A]]
    val tin = Try(new FileInputStream(file))

    try {
      for (in <- tin; xs <- Try(companion.streamFromDelimitedInput(in).toList)) yield xs
    } finally {
      tin.map(_.close())
    }
  }

  def save[A <: Msg[A]](messages: Seq[A], file: String): Try[Unit] = {
    val tout = Try(new FileOutputStream(file))
    try {
      for (out <- tout; xs <- Try(messages.foreach(x => x.writeDelimitedTo(out)))) yield xs
    } finally {
      tout.map(_.close())
    }
  }
}

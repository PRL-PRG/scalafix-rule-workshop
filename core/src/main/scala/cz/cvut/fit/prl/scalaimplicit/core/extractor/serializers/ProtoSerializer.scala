package cz.cvut.fit.prl.scalaimplicit.core.extractor.serializers

import java.io.{FileInputStream, FileOutputStream}

import resource.managed
import scalapb.GeneratedMessageCompanion

import scala.util.Try

object ProtoSerializer {
  type Message[A] = scalapb.GeneratedMessage with scalapb.Message[A]

  def load[A <: Message[A] : GeneratedMessageCompanion](file: String): Try[Seq[A]] = {
    managed(new FileInputStream(file))
      .map(in => Try(implicitly[GeneratedMessageCompanion[A]].streamFromDelimitedInput(in))).tried.flatten
  }

  def save[A <: Message[A]](messages: Seq[A], file: String): Try[Unit] = {
    managed(new FileOutputStream(file))
      .map(out => Try(messages.foreach(x => x.writeDelimitedTo(out)))).tried.flatten
  }
}

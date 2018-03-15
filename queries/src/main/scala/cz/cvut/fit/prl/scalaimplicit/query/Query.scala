package cz.cvut.fit.prl.scalaimplicit.query

import java.nio.file.Paths

import cats.effect.{IO, Sync}
import cz.cvut.fit.prl.scalaimplicit.matcher.LogicalMatchers._
import cz.cvut.fit.prl.scalaimplicit.matcher.Matcher
import fs2.{Pipe, Stream}
import io.circe
import io.circe.Decoder

import scala.language.higherKinds

object JsonQuery {
  def log[F[_], A](message: A => String): Pipe[F, A, A] =
    _.map { x =>
      println(message(x))
      x
    }

  def query[T: Decoder](input: String,
                        m: Matcher[T],
                        mx: Matcher[T]*): List[T] = {
    val combined = combineAnd(m +: mx)

    def pipeline[F[_]](implicit F: Sync[F]) =
      fs2.io.file
        .readAll[F](Paths.get(input), 4096)
        .through(circe.fs2.byteArrayParser)
        .through(circe.fs2.decoder[F, T])
        .filter(x => combined.test(x))
        .compile

    pipeline[IO].toList.unsafeRunSync()
  }

  def queryAll[T: Decoder](inputs: Seq[String],
                           m: Matcher[T],
                           mx: Matcher[T]*): List[T] = {
    val combined = combineAnd(m +: mx)

    def pipeline[F[_]](implicit F: Sync[F]) = {
      Stream
        .emits(inputs)
        .through(log(x => s"Processing $x"))
        .map(x => fs2.io.file.readAll(Paths.get(x), 4096))
        .flatMap(
          _.through(circe.fs2.byteArrayParser)
            .through(circe.fs2.decoder[F, T])
            .filter(x => combined.test(x))
        )
        .compile

    }

    pipeline[IO].toList.unsafeRunSync()
  }

}

package ai.nixiesearch.autocomplete

import ai.nixiesearch.autocomplete.model.TrainTuple
import ai.nixiesearch.autocomplete.onnx.CrossEncoder
import ai.nixiesearch.autocomplete.onnx.tokenizer.Tokenizer.SentencePair
import ai.nixiesearch.autocomplete.util.PrintProgress
import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.{readInputStream, writeOutputStream}
import io.circe.parser.*
import io.circe.syntax.*

import java.io.{File, FileInputStream, FileOutputStream}
import fs2.Stream

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Expand extends IOApp with Logging {
  override def run(args: List[String]): IO[ExitCode] = for {
    ce <- IO(
      CrossEncoder(
        new FileInputStream(
          new File("/home/shutty/work/nixiesearch/autocomplete/out_CE_big_google_canine-s/pytorch_model.onnx")
        )
      )
    )
    _ <- readInputStream[IO](
      IO(new FileInputStream(new File("/home/shutty/work/nixiesearch/autocomplete/data/train.json"))),
      1024000
    )
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .chunkN(1024)
      .unchunks
      .through(PrintProgress.tap("read lines"))
      .filter(_.nonEmpty)
      .parEvalMapUnordered(8)(line =>
        IO(decode[TrainTuple](line)).flatMap {
          case Left(value)  => IO.raiseError(value)
          case Right(value) => IO.pure(value)
        }
      )
      .chunkN(1024)
      .evalMap(c => IO(score(ce, c.toList)))
      .flatMap(c => Stream(c: _*))
      .map(x => s"${x.asJson.noSpaces}\n")
      .through(fs2.text.utf8.encode)
      .through(
        writeOutputStream[IO](
          IO(new FileOutputStream(new File("/home/shutty/work/nixiesearch/autocomplete/data/train-scored.json")))
        )
      )
      .compile
      .drain
  } yield {
    ExitCode.Success
  }

  val BATCH_SIZE = 512
  def score(ce: CrossEncoder, chunk: List[TrainTuple]) = {
    val pairs = chunk.map(t => SentencePair(t.prefix, t.target)) ++ chunk
      .flatMap(t => t.negatives.map(neg => SentencePair(t.prefix, neg)))
      .distinct
    val scores = pairs
      .grouped(BATCH_SIZE)
      .flatMap(batch => {
        val batchScores = ce.encode(batch.toArray)
        batch.zip(batchScores)
      })
      .toMap
    chunk.map(c => {
      val targetScore = scores.getOrElse(SentencePair(c.prefix, c.target), 0.0f)
      val negScores   = c.negatives.map(neg => scores.getOrElse(SentencePair(c.prefix, neg), 0.0f))
      c.copy(targetCE = Some(targetScore), negativesCE = Some(negScores.toArray))
    })
  }

}

package ai.nixiesearch.autocomplete

import ai.nixiesearch.autocomplete.fileio.Dataset
import ai.nixiesearch.autocomplete.util.{FuzzyIndex, PrintProgress}
import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.writeOutputStream

import java.io.{File, FileOutputStream}
import scala.util.Random
import io.circe.syntax.*
import fs2.Stream

object Main extends IOApp with Logging {
  val limit = Int.MaxValue
  override def run(args: List[String]): IO[ExitCode] = for {
    path      <- IO.fromOption(args.headOption)(new Exception("nope"))
    w2        <- Dataset.load(s"$path/ENG2_million.csv.zst", limit = limit)
    _         <- info("loaded MLex2")
    w3        <- Dataset.load(s"$path/ENG3_million.csv.zst", limit = limit)
    _         <- info("loaded MLex3")
    w1        <- Dataset.load(s"$path/gwordlist.csv.zst", limit = limit).map(_.take(100000))
    _         <- info("loaded gdata")
    data      <- IO(Random.shuffle(w1 ++ w2 ++ w3))
    suggester <- FuzzyIndex.create(data)
    expand <- Stream(data: _*)
      .chunkN(1024)
      .unchunks
      .through(PrintProgress.tap("rows"))
      .parEvalMapUnordered(16)(in =>
        IO(
          in.expand(suggester)
            .filter(_.negatives.nonEmpty)
            .map(row => s"${row.asJson.noSpaces}\n")
        )
      )
      .flatMap(block => Stream(block: _*))
      .through(fs2.text.utf8.encode)
      .through(writeOutputStream[IO](IO(new FileOutputStream(new File("/tmp/all.json")))))
      .compile
      .drain
  } yield {
    logger.info("completed")
    ExitCode.Success
  }

}

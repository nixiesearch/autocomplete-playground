package ai.nixiesearch.autocomplete

import ai.nixiesearch.autocomplete.fileio.Dataset
import ai.nixiesearch.autocomplete.util.VectorIndex
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import cats.effect.std.Console

object Demo extends IOApp with Logging {
  val limit = 30000

  override def run(args: List[String]): IO[ExitCode] = for {
    path  <- IO.fromOption(args.headOption)(new Exception("nope"))
    w2    <- Dataset.load(s"$path/ENG2_million.csv.zst", limit = limit)
    _     <- info("loaded MLex2")
    w3    <- Dataset.load(s"$path/ENG3_million.csv.zst", limit = limit)
    _     <- info("loaded MLex3")
    w1    <- Dataset.load(s"$path/gwordlist.csv.zst", limit = limit).map(_.take(100000))
    _     <- info("loaded gdata")
    index <- VectorIndex.index(Stream((w1 ++ w2 ++ w3).map(_.line): _*))
    _     <- loop(index)
  } yield {
    ExitCode.Success
  }

  def loop(index: VectorIndex): IO[Unit] = Console[IO].readLine.flatMap {
    case ""     => IO.unit
    case prefix => index.lookup(prefix).flatTap(lines => IO(println(lines.mkString(", ")))) *> loop(index)
  }

}

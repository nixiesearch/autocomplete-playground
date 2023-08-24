package ai.nixiesearch.autocomplete.fileio

import ai.nixiesearch.autocomplete.model.Suggestion
import cats.effect.IO
import com.github.luben.zstd.ZstdInputStream
import fs2.Stream
import fs2.io.readInputStream

import java.io.{BufferedInputStream, File, FileInputStream}

object Dataset {
  def load(path: String, sep: Char = '\t', limit: Int = Int.MaxValue): IO[List[Suggestion]] =
    CSVStream
      .fromStream(new BufferedInputStream(new ZstdInputStream(new FileInputStream(new File(path))), 1024000), '\t', 1)
      .take(limit)
      .evalMap(tokens =>
        tokens.toList match {
          case MultiLex3Format(w1, w2, w3, freq) => IO.pure(Suggestion(List(w1, w2, w3), freq))
          case MultiLex2Format(w1, w2, freq)     => IO.pure(Suggestion(List(w1, w2), freq))
          case GWordlistFormat(w1, freq)         => IO.pure(Suggestion(List(w1), freq))
          case other                             => IO.raiseError(new Exception(s"wrong format: $other"))
        }
      )
      .filter(s => !s.isStopword && (s.line.length > 2))
      .compile
      .toList
      .map(raw => {
        val list = raw
          .groupBy(s => s.terms.mkString(" ").toLowerCase())
          .map { case (lc, rows) =>
            Suggestion(lc.split(" ").toList, rows.map(_.freq).sum)
          }
          .toList
        val max = list.map(_.freq).max.toDouble
        list.map(s => s.copy(relFreq = (s.freq / max))).sortBy(-_.relFreq)
      })

  object MultiLex2Format {
    def unapply(tokens: List[String]): Option[(String, String, Long)] = tokens match {
      case ngram :: gram1 :: gram2 :: _ :: _ :: freq :: _ :: _ :: _ :: Nil =>
        Some(gram1, gram2, freq.toLong)
      case _ => None
    }
  }

  object MultiLex3Format {
    def unapply(tokens: List[String]): Option[(String, String, String, Long)] = tokens match {
      case ngram :: gram1 :: gram2 :: gram3 :: _ :: _ :: _ :: freq :: _ :: _ :: _ :: Nil =>
        Some(gram1, gram2, gram3, freq.toLong)
      case _ => None
    }
  }

  object GWordlistFormat {
    def unapply(tokens: List[String]): Option[(String, Long)] = tokens match {
      case rank :: word :: freq :: _ :: _ :: Nil =>
        Some(word, freq.replaceAll(",", "").toLong)
      case _ => None
    }
  }

}

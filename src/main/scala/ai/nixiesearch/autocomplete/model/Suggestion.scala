package ai.nixiesearch.autocomplete.model

import ai.nixiesearch.autocomplete.util.FuzzyIndex

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Suggestion(terms: List[String], freq: Long, relFreq: Double = 0.0) {
  val line = terms.mkString(" ")
  def isStopword: Boolean = {
    terms.exists(t => Suggestion.stop.contains(t))
  }
  def repeat[T](list: List[T], count: Int): List[T] = (0 until count).flatMap(_ => list).toList
  def expand(index: FuzzyIndex): List[TrainTuple] = {
    val sample = terms.mkString(" ")
    val br     = 1
    val mutated = terms match {
      case w1 :: Nil =>
        val p     = prefix(w1)
        val clean = p.map(p => TrainTuple(p, sample, index.lookup(p).filter(_ != sample)))
        val messed = for {
          p <- p.filter(_.length >= 3)
          _ <- 0 until 10
        } yield {
          val withTypo = typo(p)
          TrainTuple(withTypo, sample, index.lookup(withTypo, Some(p)).filter(_ != sample))
        }
        repeat(clean, 5) ++ messed
      case w1 :: w2 :: Nil =>
        val p1    = prefix(w1)
        val first = p1.map(p => TrainTuple(p, sample, index.lookup(p).filter(_ != sample)))
        val p2    = prefix(w2)
        val pref = p2.map(p => {
          val in = s"$w1 $p"
          TrainTuple(in, sample, index.lookup(in).filter(_ != sample))
        })
        val swapped = p1.map(p => {
          val in = s"$w2 $p"
          TrainTuple(in, sample, index.lookup(in).filter(_ != sample))
        })
        val clean = p2.map(p => {
          val in = s"${typo(w1)} $p"
          TrainTuple(in, sample, index.lookup(in, Some(s"$w1 $p")).filter(_ != sample))
        })
        val messed = p2.map(p => {
          val in = s"${typo(w1)} ${typo(p)}"
          TrainTuple(in, sample, index.lookup(in, Some(s"$w1 $p")).filter(_ != sample))
        })
        repeat(first, 3) ++ repeat(pref, 3) ++ clean ++ messed ++ swapped
      case w1 :: w2 :: w3 :: Nil =>
        val p = prefix(w3)
        val pref = p.map(p => {
          val in = s"$w1 $w2 $p"
          TrainTuple(in, sample, index.lookup(in).filter(_ != sample))
        })
        val clean = p.map(p => {
          val in = s"${typo(w1)} ${typo(w2)} $p"
          TrainTuple(in, sample, index.lookup(in, Some(s"$w1 $w2 $p")).filter(_ != sample))
        })
        val messed = p.map(p => {
          val in = s"${typo(w1)} ${typo(w2)} ${typo(p)}"
          TrainTuple(in, sample, index.lookup(in, Some(s"$w1 $w2 $p")).filter(_ != sample))
        })
        pref ++ clean ++ messed
      case _ => Nil
    }
    TrainTuple(sample, sample, index.lookup(sample).filter(_ != sample)) +: mutated.filter(_.prefix.nonEmpty)
  }

  def prefix(in: String, size: Int = 8): List[String] = {
    val buf = new ArrayBuffer[String]()
    var i   = 1
    while (i < math.min(size, in.length)) {
      buf.addOne(in.substring(0, i))
      i += 1
    }
    buf.toList
  }

  @tailrec private def typo(word: String, rounds: Int = 1): String = if (rounds == 0) { word }
  else {
    val r = Random.nextFloat()
    val result = if (r < 0.25) {
      // drop a letter
      val pos     = Random.nextInt(word.length)
      val dropped = word.substring(0, pos) ++ word.substring(pos + 1)
      dropped
    } else if (r < 0.5) {
      // duplicate a letter
      val pos  = Random.nextInt(word.length)
      val dupe = word.substring(0, pos + 1) ++ word.substring(pos)
      dupe
    } else if (r < 0.75) {
      // change a letter
      val pos = Random.nextInt(word.length)
      Suggestion.swaps.get(word.charAt(pos)) match {
        case None => word // no luck
        case Some(candidates) =>
          val repl  = candidates.charAt(Random.nextInt(candidates.length))
          val chars = word.toCharArray
          chars(pos) = repl
          new String(chars)
      }
    } else {
      // swap letters
      if (word.length > 1) {
        val pos   = Random.nextInt(word.length - 1)
        val chars = word.toCharArray
        val tmp   = chars(pos)
        chars(pos) = chars(pos + 1)
        chars(pos + 1) = tmp
        new String(chars)
      } else {
        word
      }
    }
    typo(result, rounds - 1)
  }

}

object Suggestion {
  val stop = Set(
    "a",
    "an",
    "and",
    "are",
    "as",
    "at",
    "be",
    "but",
    "by",
    "for",
    "if",
    "in",
    "into",
    "is",
    "it",
    "no",
    "not",
    "of",
    "on",
    "or",
    "such",
    "that",
    "the",
    "their",
    "then",
    "there",
    "these",
    "they",
    "this",
    "to",
    "was",
    "will",
    "with",
    "he",
    "she",
    "our",
    "her",
    "his",
    "him",
    "so",
    "my",
    "you",
    "your",
    "me",
    "do",
    "i"
  )
  val swaps = Map(
    'a' -> "qwsxz",
    'b' -> "vfghn",
    'c' -> "xdfv",
    'd' -> "serfvcx",
    'e' -> "sw34rfd",
    'f' -> "drtgbvc",
    'g' -> "ftyhbv",
    'h' -> "gyujnb",
    'i' -> "u89oklj",
    'j' -> "hyuikmn",
    'k' -> "juiolm",
    'l' -> "kiop",
    'm' -> "njk",
    'o' -> "i90plk",
    'p' -> "ol",
    'q' -> "asw",
    'r' -> "e45tgfd",
    's' -> "aqwedcxz",
    't' -> "r56yhgf",
    'u' -> "y78ikjh",
    'v' -> "cdfgb",
    'w' -> "q23edsa",
    'x' -> "zsdc",
    'y' -> "tghju76",
    'z' -> "asx"
  )

}

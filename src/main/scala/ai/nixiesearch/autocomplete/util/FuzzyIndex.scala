package ai.nixiesearch.autocomplete.util

import ai.nixiesearch.autocomplete.model.Suggestion
import ai.nixiesearch.autocomplete.Logging
import cats.effect.IO
import org.apache.lucene.analysis.{Analyzer, TokenStream, Tokenizer}
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.icu.segmentation.ICUTokenizer
import org.apache.lucene.analysis.ngram.{NGramTokenFilter, NGramTokenizer}
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.TextField
import org.apache.lucene.index.{DirectoryReader, IndexReader, IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{BooleanClause, BooleanQuery, IndexSearcher, Query, TermQuery}
import org.apache.lucene.search.suggest.InputIterator
import org.apache.lucene.search.suggest.analyzing.{AnalyzingSuggester, FuzzySuggester}
import org.apache.lucene.store.MMapDirectory
import org.apache.lucene.util.BytesRef

import java.nio.file.{Files, Paths}
import java.util
import scala.jdk.CollectionConverters.*
import scala.util.Random

case class FuzzyIndex(sug1: FuzzySuggester, sug2: FuzzySuggester, ngram: NgramIndex) {
  def lookup(prefix: String, cleanPrefix: Option[String] = None, limit: Int = 10): List[String] = {
    val out1      = sug1.lookup(prefix, false, limit).asScala.toList.map(_.key.toString)
    val out2      = sug2.lookup(prefix, false, limit).asScala.toList.map(_.key.toString)
    val ngramOut  = ngram.lookup(prefix, limit)
    val out1Clean = cleanPrefix.map(p => sug1.lookup(p, false, limit).asScala.toList.map(_.key.toString)).getOrElse(Nil)
    val br        = 1
    Random
      .shuffle(out1 ++ out2 ++ ngramOut)
      .filter(_.length > 2)
      .distinct
      // .filter(x => !x.startsWith(prefix) && !prefix.startsWith(x))
      .take(limit)
  }
}

object FuzzyIndex extends Logging {
  case class SuggestionListIterator(list: List[Suggestion]) extends InputIterator {
    val nested                                  = list.iterator
    override def weight(): Long                 = math.round(math.sqrt(current.relFreq * 1000000))
    override def payload(): BytesRef            = null
    override def hasPayloads: Boolean           = false
    override def contexts(): util.Set[BytesRef] = null
    override def hasContexts: Boolean           = false
    var current: Suggestion                     = _
    override def next(): BytesRef = if (nested.hasNext) {
      current = nested.next()
      new BytesRef(current.terms.mkString(" "))
    } else {
      null
    }
  }

  import FuzzySuggester.*
  import org.apache.lucene.search.suggest.analyzing.AnalyzingSuggester.*
  def create(words: List[Suggestion]): IO[FuzzyIndex] = IO {
    val dir  = new MMapDirectory(Files.createTempDirectory("aaa"))
    val sug1 = new FuzzySuggester(dir, "index", new EnglishAnalyzer(), new EnglishAnalyzer())
    sug1.build(SuggestionListIterator(words))
    val sug2 = new FuzzySuggester(
      dir,
      "index",
      new EnglishAnalyzer(),
      new EnglishAnalyzer(),
      EXACT_FIRST | PRESERVE_SEP,
      256,
      -1,
      true,
      2,
      DEFAULT_TRANSPOSITIONS,
      DEFAULT_NON_FUZZY_PREFIX,
      DEFAULT_MIN_FUZZY_LENGTH,
      DEFAULT_UNICODE_AWARE
    )
    sug2.build(SuggestionListIterator(words))
    val ngram = NgramIndex(dir, words)
    logger.info("suggester index loaded")
    FuzzyIndex(sug1, sug2, ngram)
  }

}

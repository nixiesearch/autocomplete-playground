package ai.nixiesearch.autocomplete.util

import ai.nixiesearch.autocomplete.model.Suggestion
import org.apache.lucene.analysis.{Analyzer, Tokenizer}
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.ngram.NGramTokenFilter
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.TextField
import org.apache.lucene.index.{DirectoryReader, IndexReader, IndexWriter, IndexWriterConfig, Term}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.{BooleanClause, BooleanQuery, IndexSearcher, Query, TermQuery}
import org.apache.lucene.store.Directory
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

case class NgramIndex(searcher: IndexSearcher, analyzer: Analyzer, reader: IndexReader) {
  def lookup(prefix: String, limit: Int = 10): List[String] = {
    search(searcher, analyzer, reader, prefix, limit).toList
  }

  def search(searcher: IndexSearcher, analyzer: Analyzer, reader: IndexReader, q: String, limit: Int) = {
    val docs = searcher.search(query(analyze(analyzer, q)), limit)
    docs.scoreDocs.map(d => reader.storedFields().document(d.doc).getField("sug").stringValue())

  }

  def analyze(a: Analyzer, query: String): List[String] = {
    val buf    = new ArrayBuffer[String]()
    val stream = a.tokenStream("sug", query)
    stream.reset()
    val term = stream.addAttribute(classOf[CharTermAttribute])
    while (stream.incrementToken()) {
      buf.addOne(term.toString)
    }
    stream.close()
    buf.toList
  }

  def query(ngrams: List[String]): Query = {
    val builder = new BooleanQuery.Builder()
    ngrams.foreach(ngram => builder.add(new BooleanClause(new TermQuery(new Term("sug", ngram)), Occur.SHOULD)))
    builder.build()
  }

}

object NgramIndex {
  def apply(dir: Directory, words: List[Suggestion]) = {
    val analyzer = new Analyzer {

      def createComponents(fieldName: String): TokenStreamComponents = {
        val source: Tokenizer = new StandardTokenizer();
        val filter            = new NGramTokenFilter(source, 3, 4, true)
        new TokenStreamComponents(source, filter);
      }
    }

    val writer = new IndexWriter(dir, new IndexWriterConfig(analyzer))
    words.foreach(s => writer.addDocument(List(new TextField("sug", s.line, Store.YES)).asJava))
    writer.commit()

    val reader   = DirectoryReader.open(writer)
    val searcher = new IndexSearcher(reader)
    new NgramIndex(searcher, analyzer, reader)
  }
}

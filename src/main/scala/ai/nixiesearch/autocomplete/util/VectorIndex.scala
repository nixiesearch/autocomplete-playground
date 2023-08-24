package ai.nixiesearch.autocomplete.util

import ai.nixiesearch.autocomplete.Logging
import ai.nixiesearch.autocomplete.onnx.BiEncoder
import cats.effect.IO
import fs2.{Chunk, Stream}
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{Document, KnnFloatVectorField, StringField}
import org.apache.lucene.index.{DirectoryReader, IndexReader, IndexWriter, IndexWriterConfig, VectorSimilarityFunction}
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.MMapDirectory

import scala.jdk.CollectionConverters.*
import java.io.{File, FileInputStream}
import java.nio.file.Paths

case class VectorIndex(searcher: IndexSearcher, bi: BiEncoder, reader: IndexReader) {
  def lookup(prefix: String): IO[List[String]] = IO {
    val query = bi.encode(Array(prefix)).head
    val docs  = searcher.search(KnnFloatVectorField.newVectorQuery("vector", query, 10), 10)
    docs.scoreDocs.map(d => reader.storedFields().document(d.doc).getField("doc").stringValue() + ":" + d.score).toList
  }
}

object VectorIndex extends Logging {
  val dir = "/home/shutty/work/nixiesearch/autocomplete/out_MultipleNegativesRankingLoss_intfloat_e5-base-v2"
  def index(data: Stream[IO, String]): IO[VectorIndex] = for {
    mmap   <- IO(new MMapDirectory(Paths.get("/tmp/vector")))
    writer <- IO(new IndexWriter(mmap, new IndexWriterConfig()))
    bienc <- IO(
      BiEncoder(
        model = new FileInputStream(new File(s"$dir/pytorch_model.onnx")),
        dic = new FileInputStream(new File(s"$dir/vocab.txt"))
      )
    )
    encoded <- data
      .chunkN(128)
      .evalMap(c =>
        IO {
          val vectors = bienc.encode(c.toArray)
          Chunk.seq(c.toList.zip(vectors))
        }
      )
      .unchunks
      .through(PrintProgress.tap("lines"))
      .parEvalMapUnordered(8)(doc =>
        IO(
          writer.addDocument(
            List(
              KnnFloatVectorField("vector", doc._2, VectorSimilarityFunction.COSINE),
              StringField("doc", doc._1, Store.YES)
            ).asJava
          )
        )
      )
      .compile
      .drain
    _        <- IO(writer.commit())
    _        <- info("index built")
    reader   <- IO(DirectoryReader.open(writer))
    searcher <- IO(new IndexSearcher(reader))

  } yield {
    VectorIndex(searcher, bienc, reader)
  }
}

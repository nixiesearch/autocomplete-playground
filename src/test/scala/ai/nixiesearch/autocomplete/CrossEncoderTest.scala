package ai.nixiesearch.autocomplete

import ai.nixiesearch.autocomplete.onnx.CrossEncoder
import ai.nixiesearch.autocomplete.onnx.tokenizer.Tokenizer.SentencePair
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{File, FileInputStream}

class CrossEncoderTest extends AnyFlatSpec with Matchers {
  it should "compute score" in {
    val model = CrossEncoder(
      new FileInputStream(new File("/home/shutty/work/nixiesearch/autocomplete/ce2_google_canine-s/pytorch_model.onnx"))
    )
    val result =
      model.encode(Array(SentencePair("a", "apple"), SentencePair("ap", "apple"), SentencePair("app", "apple"))).toList
    val br = 1
    for {
      _ <- 0 until 100000
    } yield {
      val start = System.currentTimeMillis()
      val x = model.encode(
        Array(
          SentencePair("a", "apple"),
          SentencePair("ap", "apple"),
          SentencePair("app", "apple"),
          SentencePair("appl", "apple"),
          SentencePair("a", "apple"),
          SentencePair("ap", "apple"),
          SentencePair("app", "apple"),
          SentencePair("appl", "apple"),
          SentencePair("a", "apple"),
          SentencePair("ap", "apple"),
          SentencePair("app", "apple"),
          SentencePair("appl", "apple"),
          SentencePair("a", "apple"),
          SentencePair("ap", "apple"),
          SentencePair("app", "apple"),
          SentencePair("appl", "apple"),
          SentencePair("a", "apple"),
          SentencePair("ap", "apple"),
          SentencePair("app", "apple"),
          SentencePair("appl", "apple"),
        )
      )
      val end = System.currentTimeMillis()
      println(s"${end - start}")
    }
  }
}

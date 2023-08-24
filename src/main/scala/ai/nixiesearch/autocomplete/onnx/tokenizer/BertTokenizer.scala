package ai.nixiesearch.autocomplete.onnx.tokenizer

import ai.djl.modality.nlp.bert.BertFullTokenizer
import ai.nixiesearch.autocomplete.onnx.tokenizer.Tokenizer.{SentencePair, TokenTypeMask, TransformerInput}
import ai.onnxruntime.{OnnxTensor, OrtEnvironment}
import scala.jdk.CollectionConverters.*
import java.nio.LongBuffer
import scala.collection.mutable.ArrayBuffer

case class BertTokenizer(env: OrtEnvironment, bert: BertFullTokenizer) extends Tokenizer {
  val vocab = bert.getVocabulary
  val cls   = vocab.getIndex("[CLS]")
  val sep   = vocab.getIndex("[SEP]")
  val pad   = vocab.getIndex("[PAD]")

  override def tokenize(one: String): TokenTypeMask = {
    val tokenBuffer = new ArrayBuffer[Long]()
    val typeBuffer  = new ArrayBuffer[Long]()
    val maskBuffer  = new ArrayBuffer[Long]()
    tokenBuffer.append(cls)
    typeBuffer.append(0L)
    maskBuffer.append(1L)
    bert
      .tokenize(one)
      .asScala
      .foreach(t => {
        tokenBuffer.append(vocab.getIndex(t))
        typeBuffer.append(0L)
        maskBuffer.append(1L)
      })
    tokenBuffer.append(sep)
    typeBuffer.append(0L)
    maskBuffer.append(1L)
    TokenTypeMask(tokenBuffer.toArray, typeBuffer.toArray, maskBuffer.toArray)
  }

  def tokenize(sentence: SentencePair): TokenTypeMask = {
    val tokenBuffer = new ArrayBuffer[Long]()
    val typeBuffer  = new ArrayBuffer[Long]()
    val maskBuffer  = new ArrayBuffer[Long]()
    tokenBuffer.append(cls)
    typeBuffer.append(0L)
    maskBuffer.append(1L)
    bert
      .tokenize(sentence.a)
      .asScala
      .foreach(t => {
        tokenBuffer.append(vocab.getIndex(t))
        typeBuffer.append(0L)
        maskBuffer.append(1L)
      })
    tokenBuffer.append(sep)
    typeBuffer.append(0L)
    maskBuffer.append(1L)
    bert
      .tokenize(sentence.b)
      .asScala
      .foreach(t => {
        tokenBuffer.append(vocab.getIndex(t))
        typeBuffer.append(1L)
        maskBuffer.append(1L)
      })
    tokenBuffer.append(sep)
    typeBuffer.append(1L)
    maskBuffer.append(1L)
    TokenTypeMask(tokenBuffer.toArray, typeBuffer.toArray, maskBuffer.toArray)
  }

}

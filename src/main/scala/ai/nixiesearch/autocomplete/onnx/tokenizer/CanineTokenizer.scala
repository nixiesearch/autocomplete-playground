package ai.nixiesearch.autocomplete.onnx.tokenizer

import ai.nixiesearch.autocomplete.onnx.tokenizer.Tokenizer.TokenTypeMask
import ai.onnxruntime.OrtEnvironment

import scala.collection.mutable.ArrayBuffer

case class CanineTokenizer(env: OrtEnvironment) extends Tokenizer {
  override val pad: Long = 0L
  val cls                = 0xe000
  val sep                = 0xe001

  override def tokenize(one: String): TokenTypeMask = ???

  override def tokenize(pair: Tokenizer.SentencePair): Tokenizer.TokenTypeMask = {
    val tokenBuffer = new ArrayBuffer[Long]()
    val typeBuffer  = new ArrayBuffer[Long]()
    val maskBuffer  = new ArrayBuffer[Long]()
    tokenBuffer.append(cls)
    typeBuffer.append(0L)
    maskBuffer.append(1L)
    pair.a.toCharArray
      .foreach(c => {
        tokenBuffer.append(c.toLong)
        typeBuffer.append(0L)
        maskBuffer.append(1L)
      })
    tokenBuffer.append(sep)
    typeBuffer.append(0L)
    maskBuffer.append(1L)
    pair.b.toCharArray
      .foreach(c => {
        tokenBuffer.append(c.toLong)
        typeBuffer.append(1L)
        maskBuffer.append(1L)
      })
    tokenBuffer.append(sep)
    typeBuffer.append(1L)
    maskBuffer.append(1L)
    TokenTypeMask(tokenBuffer.toArray, typeBuffer.toArray, maskBuffer.toArray)
  }
}

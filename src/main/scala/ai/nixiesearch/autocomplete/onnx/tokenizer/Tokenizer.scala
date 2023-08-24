package ai.nixiesearch.autocomplete.onnx.tokenizer

import ai.nixiesearch.autocomplete.onnx.tokenizer.Tokenizer.{SentencePair, TokenTypeMask, TransformerInput}
import ai.onnxruntime.{OnnxTensor, OrtEnvironment}

import java.nio.LongBuffer

trait Tokenizer {
  def env: OrtEnvironment
  def pad: Long
  def tokenize(one: String): TokenTypeMask
  def tokenize(pair: SentencePair): TokenTypeMask

  def tokenizeBatch(strings: Array[String]): Tokenizer.TransformerInput = {
    val encoded = strings.map(sp => tokenize(sp))
    val maxLength = encoded.map(_.tokens.length).max
    val tensorSize = strings.length * maxLength
    val tokens = new Array[Long](tensorSize)
    val tokenTypes = new Array[Long](tensorSize)
    val attMask = new Array[Long](tensorSize)

    var s = 0
    var i = 0
    while (s < strings.length) {
      var j = 0
      while (j < maxLength) {
        if (j < encoded(s).tokens.length) {
          tokens(i) = encoded(s).tokens(j)
          tokenTypes(i) = encoded(s).types(j)
          attMask(i) = encoded(s).attmask(j)
        } else {
          tokens(i) = pad
          tokenTypes(i) = pad
          attMask(i) = pad
        }
        i += 1
        j += 1
      }
      s += 1
    }
    val tensorDim = Array(strings.length.toLong, maxLength.toLong)
    val args = Map(
      "input_ids" -> OnnxTensor.createTensor(env, LongBuffer.wrap(tokens), tensorDim),
      "token_type_ids" -> OnnxTensor.createTensor(env, LongBuffer.wrap(tokenTypes), tensorDim),
      "attention_mask" -> OnnxTensor.createTensor(env, LongBuffer.wrap(attMask), tensorDim)
    )
    TransformerInput(args)

  }

  def tokenizeBatch(strings: Array[SentencePair]): Tokenizer.TransformerInput = {
    val encoded    = strings.map(sp => tokenize(sp))
    val maxLength  = encoded.map(_.tokens.length).max
    val tensorSize = strings.length * maxLength
    val tokens     = new Array[Long](tensorSize)
    val tokenTypes = new Array[Long](tensorSize)
    val attMask    = new Array[Long](tensorSize)

    var s = 0
    var i = 0
    while (s < strings.length) {
      var j = 0
      while (j < maxLength) {
        if (j < encoded(s).tokens.length) {
          tokens(i) = encoded(s).tokens(j)
          tokenTypes(i) = encoded(s).types(j)
          attMask(i) = encoded(s).attmask(j)
        } else {
          tokens(i) = pad
          tokenTypes(i) = pad
          attMask(i) = pad
        }
        i += 1
        j += 1
      }
      s += 1
    }
    val tensorDim = Array(strings.length.toLong, maxLength.toLong)
    val args = Map(
      "input_ids"      -> OnnxTensor.createTensor(env, LongBuffer.wrap(tokens), tensorDim),
      "token_type_ids" -> OnnxTensor.createTensor(env, LongBuffer.wrap(tokenTypes), tensorDim),
      "attention_mask" -> OnnxTensor.createTensor(env, LongBuffer.wrap(attMask), tensorDim)
    )
    TransformerInput(args)
  }

}

object Tokenizer {
  case class SentencePair(a: String, b: String)
  case class TransformerInput(tensors: Map[String, OnnxTensor])
  case class TokenTypeMask(tokens: Array[Long], types: Array[Long], attmask: Array[Long])
}

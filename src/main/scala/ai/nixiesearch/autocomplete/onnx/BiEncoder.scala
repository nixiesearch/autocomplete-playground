package ai.nixiesearch.autocomplete.onnx

import ai.djl.modality.nlp.DefaultVocabulary
import ai.djl.modality.nlp.bert.BertFullTokenizer
import ai.nixiesearch.autocomplete.Logging
import ai.nixiesearch.autocomplete.onnx.tokenizer.Tokenizer
import ai.onnxruntime.OrtSession.SessionOptions
import ai.onnxruntime.OrtSession.SessionOptions.OptLevel
import ai.onnxruntime.{OnnxTensor, OrtEnvironment, OrtSession}
import org.apache.commons.io.{FileUtils, IOUtils}

import java.io.InputStream
import scala.jdk.CollectionConverters.*
import java.nio.LongBuffer
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer

case class BiEncoder(env: OrtEnvironment, session: OrtSession, tokenizer: BertFullTokenizer, dim: Int) {
  val vocab = tokenizer.getVocabulary
  val cls   = vocab.getIndex("[CLS]")
  val sep   = vocab.getIndex("[SEP]")
  val pad   = vocab.getIndex("[PAD]")

  def encode(batch: Array[String]): Array[Array[Float]] = {
    val textTokens = batch.map(sentence => tokenize(sentence))
    val maxLength  = textTokens.map(_.length).max
    val tensorSize = batch.length * maxLength
    val tokens     = new Array[Long](tensorSize)
    val tokenTypes = new Array[Long](tensorSize)
    val attMask    = new Array[Long](tensorSize)

    var s = 0
    var i = 0
    while (s < batch.length) {
      var j = 0
      while (j < math.max(maxLength, textTokens(s).length)) {
        if (j < textTokens(s).length) {
          tokens(i) = textTokens(s)(j)
          tokenTypes(i) = 0 // ???
          attMask(i) = 1
        } else {
          tokens(i) = pad
          tokenTypes(i) = 0
          attMask(i) = 0
        }
        i += 1
        j += 1
      }
      s += 1
    }
    val tensorDim = Array(batch.length.toLong, maxLength.toLong)
    val args = Map(
      "input_ids"      -> OnnxTensor.createTensor(env, LongBuffer.wrap(tokens), tensorDim),
      "token_type_ids" -> OnnxTensor.createTensor(env, LongBuffer.wrap(tokenTypes), tensorDim),
      "attention_mask" -> OnnxTensor.createTensor(env, LongBuffer.wrap(attMask), tensorDim)
    )
    val result     = session.run(args.asJava)
    val tensor     = result.get(0).getValue.asInstanceOf[Array[Array[Array[Float]]]]
    val normalized = avgpool(tensor, textTokens, dim)
    result.close()
    args.values.foreach(_.close())
    normalized
  }

  def tokenize(sentence: String): Array[Long] = {
    val buffer = new ArrayBuffer[Long]()
    buffer.append(cls)
    tokenizer
      .tokenize(sentence)
      .asScala
      .foreach(t => {
        buffer.append(vocab.getIndex(t))
      })
    buffer.append(sep)
    buffer.toArray
  }

  def avgpool(tensor: Array[Array[Array[Float]]], tokens: Array[Array[Long]], dim: Int): Array[Array[Float]] = {
    val result = new Array[Array[Float]](tokens.length)
    var s      = 0
    while (s < tensor.length) {
      val embed = new Array[Float](dim)
      var i     = 0
      while (i < dim) {
        var sum = 0.0
        var cnt = 0
        var j   = 0
        while (j < tensor(s).length) {
          if (j < tokens(s).length) {
            sum += tensor(s)(j)(i)
            cnt += 1
          }
          j += 1
        }
        embed(i) = (sum / cnt).toFloat
        i += 1
      }
      result(s) = embed
      s += 1
    }
    result
  }

}

object BiEncoder extends Logging {
  def apply(model: InputStream, dic: InputStream) = {
    val vocab =
      DefaultVocabulary.builder().add(IOUtils.toString(dic, StandardCharsets.UTF_8).split('\n').toList.asJava).build()
    val tokenizer = new BertFullTokenizer(vocab, true)

    val env  = OrtEnvironment.getEnvironment("sbert")
    val opts = new SessionOptions()
    opts.setIntraOpNumThreads(Runtime.getRuntime.availableProcessors())
    opts.setOptimizationLevel(OptLevel.ALL_OPT)
    opts.addCUDA(0)
    val modelBytes = IOUtils.toByteArray(model)
    val session    = env.createSession(modelBytes, opts)
    val size       = FileUtils.byteCountToDisplaySize(modelBytes.length)
    val inputs     = session.getInputNames.asScala.toList
    val outputs    = session.getOutputNames.asScala.toList
    logger.info(s"Loaded ONNX model (size=$size inputs=$inputs outputs=$outputs)")
    new BiEncoder(env, session, tokenizer, 768)
  }
}

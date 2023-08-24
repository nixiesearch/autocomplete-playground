package ai.nixiesearch.autocomplete.onnx

import ai.nixiesearch.autocomplete.Logging
import ai.nixiesearch.autocomplete.onnx.tokenizer.{CanineTokenizer, Tokenizer}
import ai.nixiesearch.autocomplete.onnx.tokenizer.Tokenizer.SentencePair
import ai.onnxruntime.OrtSession.SessionOptions
import ai.onnxruntime.OrtSession.SessionOptions.OptLevel
import ai.onnxruntime.{OrtEnvironment, OrtSession}
import org.apache.commons.io.{FileUtils, IOUtils}

import java.io.InputStream
import scala.jdk.CollectionConverters.*

case class CrossEncoder(env: OrtEnvironment, session: OrtSession, tokenizer: Tokenizer) {
  def encode(batch: Array[SentencePair]): Array[Float] = {
    if (batch.length == 0) {
      Array.empty
    } else {
      val args   = tokenizer.tokenizeBatch(batch)
      val result = session.run(args.tensors.asJava)
      val tensor = result.get(0).getValue.asInstanceOf[Array[Array[Float]]]
      val logits = new Array[Float](batch.length)
      var j      = 0
      while (j < batch.length) {
        logits(j) = 1.0f / (1.0f + math.exp(-tensor(j)(0)).toFloat)
        j += 1
      }
      result.close()
      args.tensors.values.foreach(_.close())
      logits
    }
  }
}
object CrossEncoder extends Logging {
  def apply(model: InputStream) = {
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
    new CrossEncoder(env, session, CanineTokenizer(env))
  }
}

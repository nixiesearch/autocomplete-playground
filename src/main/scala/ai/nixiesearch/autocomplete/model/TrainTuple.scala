package ai.nixiesearch.autocomplete.model

import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}

case class TrainTuple(
    prefix: String,
    target: String,
    negatives: List[String],
    targetCE: Option[Float] = None,
    negativesCE: Option[Array[Float]] = None
)

object TrainTuple {
  given tupleEncoder: Encoder[TrainTuple] = deriveEncoder[TrainTuple]
  given tupleDecoder: Decoder[TrainTuple] = deriveDecoder[TrainTuple]
}

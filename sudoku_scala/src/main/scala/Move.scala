import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Move(idx: Int, isError: Boolean, number: Int)

object Move {
  implicit val decoder: Decoder[Move] = deriveDecoder
  implicit val encoder: Encoder[Move] = deriveEncoder
}
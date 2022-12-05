import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode

sealed trait GameState

final case class InMenuState() extends GameState

final case class InGameState() extends GameState

object InGameState {
  val STORAGE_KEY_NAME = "GAME_STATE"

  implicit val decoder: Decoder[InGameState] = deriveDecoder
  implicit val encoder: Encoder[InGameState] = deriveEncoder

  def apply(json: String): Option[InGameState] = {
    decode[InGameState](json) match {
      case Left(_) => None
      case Right(gameState) => Some(gameState)
    }
  }
}

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode

sealed trait GameState

final case class InMenuState() extends GameState

final case class InGameState() extends GameState

object GameState {
  val STORAGE_KEY_NAME = "GAME_STATE"

  implicit val decoder: Decoder[InGameState] = deriveDecoder
  implicit val encoder: Encoder[InGameState] = deriveEncoder

  def apply(json: String): GameState = {
    decode[InGameState](json) match {
      case Left(ex) =>
        ex.printStackTrace()
        InMenuState()
      case Right(gameState) => gameState
    }
  }
}

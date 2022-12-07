import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

sealed trait GameState

case object InMenuState extends GameState

final case class InGameState(grid: Grid, complexity: Complexity) extends GameState

object InGameState {
  val STORAGE_KEY_NAME = "GAME_STATE"

  implicit val decoder: Decoder[InGameState] = deriveDecoder
  implicit val encoder: Encoder[InGameState] = deriveEncoder

  def apply(complexity: Complexity): InGameState = {
    val grid = Complexity.changeSolvedGridByComplexity(Grid.solve(Grid().cells), complexity)
    InGameState(grid, complexity)
  }

  def apply(json: String): Option[InGameState] = {
    decode[InGameState](json) match {
      case Left(ex) =>
        ex.printStackTrace()
        None
      case Right(gameState) => Some(gameState)
    }
  }

  def toJson(state: InGameState): String = {
    state.asJson.noSpaces
  }
}

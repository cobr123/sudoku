import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.decode
import io.circe.syntax.EncoderOps

import scala.collection.mutable

sealed trait GameState

case object InMenuState extends GameState

final case class InGameState(grid: Grid,
                             complexity: Complexity,
                             var selectedIdx: Option[Int] = None,
                             moveHistory: mutable.ArrayBuffer[Move] = mutable.ArrayBuffer.empty,
                            ) extends GameState

object InGameState {
  val STORAGE_KEY_NAME = "GAME_STATE"

  implicit val config: Configuration = Configuration.default.withDefaults

  implicit val decoder: Decoder[InGameState] = deriveConfiguredDecoder
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

  def toJsonDebug(state: InGameState): String = {
    state.asJson.spaces2SortKeys
  }
}

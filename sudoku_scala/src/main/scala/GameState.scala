import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, readFromString, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

sealed trait GameState

case object InMenuState extends GameState

final case class InGameState(grid: Grid,
                             complexity: Complexity,
                             var selectedIdx: Option[Int] = None,
                             var ghostMode: Boolean = false,
                             moveHistory: mutable.ArrayBuffer[Move] = mutable.ArrayBuffer.empty,
                             guesses: mutable.HashMap[Int, Set[Int]] = mutable.HashMap.empty,
                            ) extends GameState

object InGameState {
  val STORAGE_KEY_NAME = "GAME_STATE"

  implicit val codec: JsonValueCodec[InGameState] = JsonCodecMaker.make

  def apply(complexity: Complexity): InGameState = {
    val grid = Complexity.changeSolvedGridByComplexity(Grid.solve(Grid().cells), complexity)
    InGameState(grid, complexity)
  }

  def apply(json: String): Option[InGameState] = {
    Try(readFromString[InGameState](json)) match {
      case Failure(ex) =>
        ex.printStackTrace()
        None
      case Success(gameState) => Some(gameState)
    }
  }

  def toJson(state: InGameState): String = {
    writeToString(state)
  }

  def toJsonDebug(state: InGameState): String = {
    writeToString(state)
  }
}

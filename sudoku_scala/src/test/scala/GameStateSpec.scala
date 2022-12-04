import io.circe.syntax.EncoderOps
import org.scalatest.funspec.AnyFunSpec

class GameStateSpec extends AnyFunSpec{

  it("encode and decode InGameState") {
    val inGameState = InGameState()
    val state = GameState(inGameState.asJson.noSpaces)
    assert(inGameState === state)
  }

  it("encode and decode InMenuState") {
    val state = GameState("")
    assert(InMenuState() === state)
  }
}

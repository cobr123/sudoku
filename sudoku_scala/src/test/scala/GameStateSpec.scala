import io.circe.syntax.EncoderOps
import org.scalatest.funspec.AnyFunSpec

class GameStateSpec extends AnyFunSpec{

  it("encode and decode InGameState") {
    val inGameState = InGameState()
    val state = InGameState(inGameState.asJson.noSpaces)
    assert(Some(inGameState) === state)
  }

  it("encode and decode InMenuState") {
    val state = InGameState("")
    assert(None === state)
  }
}

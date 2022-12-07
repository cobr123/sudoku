import io.circe.syntax.EncoderOps
import org.scalatest.funspec.AnyFunSpec

class GameStateSpec extends AnyFunSpec {

  private def getSolvedGrid: Grid = {
    val r1 = Row(5, 3, 4, 6, 7, 8, 9, 1, 2)
    val r2 = Row(6, 7, 2, 1, 9, 5, 3, 4, 8)
    val r3 = Row(1, 9, 8, 3, 4, 2, 5, 6, 7)
    val r4 = Row(8, 5, 9, 7, 6, 1, 4, 2, 3)
    val r5 = Row(4, 2, 6, 8, 5, 3, 7, 9, 1)
    val r6 = Row(7, 1, 3, 9, 2, 4, 8, 5, 6)
    val r7 = Row(9, 6, 1, 5, 3, 7, 2, 8, 4)
    val r8 = Row(2, 8, 7, 4, 1, 9, 6, 3, 5)
    val r9 = Row(3, 4, 5, 2, 8, 6, 1, 7, 9)
    val grid = Grid(Array(r1, r2, r3, r4, r5, r6, r7, r8, r9))
    assert(Grid.isFinished(grid.cells))
    grid
  }

  it("encode and decode InGameState") {
    val inGameState = InGameState(getSolvedGrid, Complexity.Easy)
    val state = InGameState(inGameState.asJson.noSpaces)
    println(inGameState.asJson.noSpaces)
    assert(Some(inGameState) === state)
  }

  it("encode and decode InMenuState") {
    val state = InGameState("")
    assert(None === state)
  }
}

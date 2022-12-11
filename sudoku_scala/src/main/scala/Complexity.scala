
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import scala.util.Random

enum Complexity(val solvedCellCount: Int) {

  case Easy extends Complexity(50)

  case Medium extends Complexity(38)

  case Hard extends Complexity(28)

  case Expert extends Complexity(22)

  case Extreme extends Complexity(17)
}

object Complexity {

  implicit val codec: JsonValueCodec[Complexity] = JsonCodecMaker.makeWithoutDiscriminator

  def changeSolvedGridByComplexity(grid: Grid, complexity: Complexity): Grid = {
    assert(Grid.isFinished(grid.cells))
    val solvedIndexes = Random.shuffle(grid.cells.indices.toList).take(complexity.solvedCellCount).toSet
    grid.cells.zipWithIndex.foreach {
      case (_, idx) => if (!solvedIndexes.contains(idx)) {
        grid.cells(idx) = 0
      }
    }
    assert(!Grid.isFinished(grid.cells))
    grid
  }
}
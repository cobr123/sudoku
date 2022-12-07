import enumeratum._

import scala.util.Random

sealed abstract class Complexity(val solvedCellCount: Int) extends EnumEntry

object Complexity extends Enum[Complexity] with CirceEnum[Complexity] {
  val values = findValues

  case object Easy extends Complexity(50)

  case object Medium extends Complexity(38)

  case object Hard extends Complexity(28)

  case object Expert extends Complexity(22)

  case object Extreme extends Complexity(17)

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
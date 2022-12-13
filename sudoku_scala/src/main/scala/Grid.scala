
import scala.annotation.tailrec
import scala.util.Random

final case class Grid(cells: Array[Int]) {
  assert(cells.length == 9 * 9)
}

object Grid {

  def apply(subGrids: SubGrid*): Grid = {
    val rows = List(0, 3, 6).flatMap { i =>
      subGrids.slice(i, i + 3).flatMap(sg => SubGrid.getRow(sg.nums)(0)) ++
        subGrids.slice(i, i + 3).flatMap(sg => SubGrid.getRow(sg.nums)(1)) ++
        subGrids.slice(i, i + 3).flatMap(sg => SubGrid.getRow(sg.nums)(2))
    }
    this (rows.toArray)
  }

  def apply(rows: Array[Row]): Grid = {
    this (rows.flatMap(_.nums))
  }

  def apply(): Grid = Grid(Array.fill(9 * 9)(0))

  private val colIndexes: Array[Int] = (0 until 9).toArray
  private val rowIndexes: Array[Int] = (0 until 9).toArray
  private val subGridIndexes: Array[Int] = (0 until 3).toArray

  def isFinished(cells: Array[Int]): Boolean = {
    colIndexes.map(getCol(cells)).forall(SubGrid.isCorrect) &&
      rowIndexes.map(getRow(cells)).forall(SubGrid.isCorrect) &&
      subGridIndexes.map(getSubGrid(cells)).forall(SubGrid.isCorrect)
  }

  def getSubGrid(cells: Array[Int])(subGridIdx: Int): Array[Int] = {
    val numberOfColumns = 3
    val column = subGridIdx % numberOfColumns
    val row = (subGridIdx - column) / numberOfColumns

    val idx = (row * 9 + column) * 3
    cells.slice(idx, idx + 3) ++
      cells.slice(idx + 9, idx + 9 + 3) ++
      cells.slice(idx + 9 + 9, idx + 9 + 9 + 3)
  }

  def getCol(cells: Array[Int])(col: Int): Array[Int] = {
    cells.sliding(9, 9).map(_.apply(col)).toArray
  }

  def getRow(cells: Array[Int])(row: Int): Array[Int] = {
    val from = row * 9
    val until = from + 9
    cells.slice(from, until)
  }

  private def updateByIdx(arr: Array[Int], idx: Int, available: Array[Int]): Boolean = {
    if (available.isEmpty) {
      false
    } else {
      arr(idx) = Random.shuffle(available).head
      true
    }
  }

  private def getSubGridIdx(gridRow: Int, gridCol: Int): Int = {
    val idx = (gridRow, gridCol) match {
      case (row, col) if row >= 0 && row < 3 && col >= 0 && col < 3 => 0
      case (row, col) if row >= 0 && row < 3 && col >= 3 && col < 6 => 1
      case (row, col) if row >= 0 && row < 3 && col >= 6 && col < 9 => 2
      case (row, col) if row >= 3 && row < 6 && col >= 0 && col < 3 => 3
      case (row, col) if row >= 3 && row < 6 && col >= 3 && col < 6 => 4
      case (row, col) if row >= 3 && row < 6 && col >= 6 && col < 9 => 5
      case (row, col) if row >= 6 && row < 9 && col >= 0 && col < 3 => 6
      case (row, col) if row >= 6 && row < 9 && col >= 3 && col < 6 => 7
      case (row, col) if row >= 6 && row < 9 && col >= 6 && col < 9 => 8
    }
    //println(gridRow, gridCol, idx)
    assert(idx >= 0 && idx < 9)
    idx
  }

  val availableValues = (1 to 9).toSet

  @tailrec
  private def trySolveRandom(gridCells: Array[Int], idx: Int = 0): Array[Int] = {
    if (idx < gridCells.length) {
      if (gridCells(idx) == 0) {
        val numberOfColumns = 9
        val column = idx % numberOfColumns
        val row = (idx - column) / numberOfColumns
        val subGridIdx = getSubGridIdx(row, column)
        val newFilter = availableValues.removedAll(
          getCol(gridCells)(column) ++
            getRow(gridCells)(row) ++
            getSubGrid(gridCells)(subGridIdx)
        ).toArray
        updateByIdx(gridCells, idx, newFilter)
      }
      trySolveRandom(gridCells, idx + 1)
    } else {
      gridCells
    }
  }

  @tailrec
  def solve(initGridCells: Array[Int], lastPrintTimeMillis: Long = 0): Grid = {
    val cells = trySolveRandom(initGridCells.clone())

    if (isFinished(cells)) {
      Grid(cells)
    } else {
      var printTimeMillis = lastPrintTimeMillis
      val currentTimeMillis = System.currentTimeMillis()
      if (currentTimeMillis - lastPrintTimeMillis >= 5000) {
        printGrid(cells)
        printTimeMillis = currentTimeMillis
      }
      solve(initGridCells, printTimeMillis)
    }
  }

  def printGrid(cells: Array[Int]): Unit = {
    println("------------------------------------------------------")
    rowIndexes.map(getRow(cells)(_).mkString("", " ", "")).foreach(println)
  }

  def placeNumber(cells: Array[Int], idx: Int, number: Int): Boolean = {
    cells(idx) = number
    Random.nextBoolean()
  }

  def getGuesses(cells: Array[Int], idx: Int): Set[Int] = {
    val numberOfColumns = 9
    val column = idx % numberOfColumns
    val row = (idx - column) / numberOfColumns

    availableValues -- getRow(cells)(row) -- getCol(cells)(column) -- getSubGrid(cells)(getSubGridIdx(row, column))
  }
}
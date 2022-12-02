import scala.annotation.tailrec
import scala.util.Random

case class Row(nums: Array[Int]) {
  assert(nums.length == 3 * 3)
  assert(nums.forall(num => num >= 0 && num <= 9))
}

object Row {
  def apply(nums: Int*): Row = this (nums.toArray)

  def generateCompleted: Row = {
    val nums = Random.shuffle((1 to 9).toList).toArray
    Row(nums)
  }
}

case class SubGrid(nums: Array[Int]) {
  assert(nums.length == 3 * 3)
  assert(nums.forall(num => num >= 0 && num <= 9))
}

object SubGrid {
  def apply(nums: Int*): SubGrid = this (nums.toArray)

  def isFilledCorrect(sg: SubGrid): Boolean = {
    isCorrect(sg.nums)
  }

  def isCorrect(nums: Array[Int]): Boolean = {
    nums.filter(_ > 0).distinct.length == 9
  }

  def getCol(nums: Array[Int])(col: Int): Array[Int] = {
    (col, nums) match {
      case (0, Array(
      a, _, _,
      b, _, _,
      c, _, _
      )) => Array(a, b, c)
      case (1, Array(
      _, a, _,
      _, b, _,
      _, c, _
      )) => Array(a, b, c)
      case (2, Array(
      _, _, a,
      _, _, b,
      _, _, c
      )) => Array(a, b, c)
    }
  }

  def getRow(nums: Array[Int])(row: Int): Array[Int] = {
    (row, nums) match {
      case (0, Array(
      a, b, c,
      _, _, _,
      _, _, _
      )) => Array(a, b, c)
      case (1, Array(
      _, _, _,
      a, b, c,
      _, _, _
      )) => Array(a, b, c)
      case (2, Array(
      _, _, _,
      _, _, _,
      a, b, c
      )) => Array(a, b, c)
    }
  }

  def generateCompleted: SubGrid = {
    val nums = Random.shuffle((1 to 9).toList).toArray
    SubGrid(nums)
  }

  def generateRowPart(filter: Set[Int]): Array[Int] = {
    Random.shuffle((1 to 9).filterNot(filter.contains))
      .take(3)
      .toArray
  }

  def printSubGrid(nums: Array[Int]): Unit = {
    println("------------------------------------------------------")
    (0 to 2).map(getRow(nums)(_).mkString("", " ", "")).foreach(println)
  }
}

case class Grid(cells: Array[Int]) {
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

  private val availableValues = (1 to 9).toSet

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
      if (System.currentTimeMillis() - lastPrintTimeMillis >= 5000) {
        printGrid(cells)
        printTimeMillis = System.currentTimeMillis()
      }
      solve(initGridCells, printTimeMillis)
    }
  }

  def printGrid(cells: Array[Int]): Unit = {
    println("------------------------------------------------------")
    rowIndexes.map(getRow(cells)(_).mkString("", " ", "")).foreach(println)
  }
}
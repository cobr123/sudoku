
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
  private val gridIndexes: Array[Int] = (0 until 9 * 9).toArray
  val subGridIndexes: Array[Int] = (0 until 9).toArray

  def isFinished(cells: Array[Int]): Boolean = {
    colIndexes.map(getCol(cells)).forall(SubGrid.isCorrect) &&
      rowIndexes.map(getRow(cells)).forall(SubGrid.isCorrect) &&
      subGridIndexes.map(getSubGrid(cells)).forall(SubGrid.isCorrect)
  }

  def getSubGridRowAndColumn(subGridIdx: Int): (Int, Int) = {
    val numberOfColumns = 3
    val column = subGridIdx % numberOfColumns
    val row = (subGridIdx - column) / numberOfColumns
    (row, column)
  }

  def getSubGridCellIdxs(subGridIdx: Int): Array[Int] = {
    val (row, column) = getSubGridRowAndColumn(subGridIdx)

    val idx = (row * 9 + column) * 3
    gridIndexes.slice(idx, idx + 3) ++
      gridIndexes.slice(idx + 9, idx + 9 + 3) ++
      gridIndexes.slice(idx + 9 + 9, idx + 9 + 9 + 3)
  }

  def getSubGrid(cells: Array[Int])(subGridIdx: Int): Array[Int] = {
    getSubGridCellIdxs(subGridIdx).map(idx => cells(idx))
  }

  def getColIdxs(col: Int): Array[Int] = {
    gridIndexes.sliding(9, 9).map(_.apply(col)).toArray
  }

  def getCol(cells: Array[Int])(col: Int): Array[Int] = {
    getColIdxs(col).map(idx => cells(idx))
  }

  def getRowIdxs(row: Int): Array[Int] = {
    val from = row * 9
    val until = from + 9
    (from until until).toArray
  }

  def getRow(cells: Array[Int])(row: Int): Array[Int] = {
    getRowIdxs(row).map(idx => cells(idx))
  }

  private def updateByIdx(arr: Array[Int], idx: Int, available: Array[Int]): Boolean = {
    if (available.isEmpty) {
      false
    } else {
      arr(idx) = Random.shuffle(available).head
      true
    }
  }

  def getSubGridIdx(gridRow: Int, gridCol: Int): Int = {
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

  val availableValues: Set[Int] = (1 to 9).toSet

  @tailrec
  private def trySolveRandom(gridCells: Array[Int], idx: Int = 0): Array[Int] = {
    if (idx < gridCells.length) {
      if (gridCells(idx) == 0) {
        val (row, column) = getRowAndColumn(idx)
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

  def printGrid(cells: Array[Int], idx: Int): Unit = {
    val (row, column) = getRowAndColumn(idx)
    println("------------------------------------------------------")
    rowIndexes
      .map { r =>
        val arr = if (r == row) {
          getRow(cells)(r).zipWithIndex.map {
            case (n, c) if c == column => s"[$n]"
            case (n, _) => s"$n"
          }
        } else {
          getRow(cells)(r)
        }
        arr.mkString("", " ", "")
      }
      .foreach(println)
  }

  def getCanPlace(cells: Array[Int], idx: Int, number: Int): Boolean = {
    getGuesses(cells, idx).contains(number)
  }

  def placeNumber(cells: Array[Int], idx: Int, number: Int): Boolean = {
    val canPlace = getCanPlace(cells, idx, number)
    cells(idx) = number
    canPlace
  }

  def getRowAndColumn(idx: Int): (Int, Int) = {
    val numberOfColumns = 9
    val column = idx % numberOfColumns
    val row = (idx - column) / numberOfColumns
    (row, column)
  }

  def getGuesses(cells: Array[Int], idx: Int): Set[Int] = {
    val (row, column) = getRowAndColumn(idx)
    availableValues -- getRow(cells)(row) -- getCol(cells)(column) -- getSubGrid(cells)(getSubGridIdx(row, column))
  }

  @tailrec
  def getLastNumberGuessInSubGrid(guesses: Map[Int, Set[Int]], subGridIdxs: Array[Int] = Grid.subGridIndexes): Option[(Int, Set[Int])] = {
    if (guesses.isEmpty || subGridIdxs.isEmpty) {
      None
    } else {
      Grid.getSubGridCellIdxs(subGridIdxs.head)
        .flatMap(idx => guesses.getOrElse(idx, Set.empty).toArray.map(num => (num, Set(idx))))
        .groupBy(_._1)
        .find {
          case (_, arr) if arr.length == 1 && arr.head._2.size == 1 => true
          case _ => false
        } match {
        case Some((num, arr)) =>
          Some((arr.head._2.head, Set(num)))
        case None =>
          getLastNumberGuessInSubGrid(guesses, subGridIdxs.tail)
      }
    }
  }

  def getHighlightedIds(inGameState: InGameState, number: Int): Array[String] = {
    if (number > 0) {
      val numberIds = inGameState.grid.cells.zipWithIndex.filter {
        case (n, _) => n == number
      }.map {
        case (_, idx) => s"cell_$idx"
      }
      val guessIds = inGameState.guesses.filter {
        case (_, s) => s.contains(number)
      }.map {
        case (parentIdx, _) => s"cell_${parentIdx}_$number"
      }
      numberIds ++ guessIds
    } else {
      Array.empty
    }
  }

  def getGhostCrossedByLine(guesses: Map[Int, Set[Int]]): Option[(Int, Int)] = {
    if (guesses.isEmpty) {
      None
    } else {
      Grid.subGridIndexes match {
        case Array(
        sgi0, sgi1, sgi2,
        sgi3, sgi4, sgi5,
        sgi6, sgi7, sgi8
        ) =>
          List(
            (sgi0, sgi1), (sgi0, sgi2), (sgi1, sgi2),
            (sgi3, sgi4), (sgi3, sgi5), (sgi4, sgi5),
            (sgi6, sgi7), (sgi6, sgi8), (sgi7, sgi8),
          ).foreach {
            case (sgi0, sgi1) =>
              Grid.availableValues.foreach { number =>
                val subGrid0 = Grid.getSubGridCellIdxs(sgi0)
                val subGrid1 = Grid.getSubGridCellIdxs(sgi1)
                val subGrid0Bools = subGrid0.map(idx => guesses.getOrElse(idx, Set.empty).contains(number))
                val subGrid1Bools = subGrid1.map(idx => guesses.getOrElse(idx, Set.empty).contains(number))

                val res = (getSubGridLine(subGrid0Bools), getSubGridLine(subGrid1Bools)) match {
                  case (Some(SubGridLine.Horizontal(row)), _) => getGuessIdxByNumber(SubGrid.getRow(subGrid1)(row), guesses, number)
                  case (_, Some(SubGridLine.Horizontal(row))) => getGuessIdxByNumber(SubGrid.getRow(subGrid0)(row), guesses, number)
                  case _ => None
                }
                if (res.isDefined) {
                  return res
                }
              }
          }
          List(
            (sgi0, sgi3), (sgi0, sgi6), (sgi3, sgi6),
            (sgi1, sgi4), (sgi1, sgi7), (sgi4, sgi7),
            (sgi2, sgi5), (sgi2, sgi8), (sgi5, sgi8),
          ).foreach {
            case (sgi0, sgi1) =>
              Grid.availableValues.foreach { number =>
                val subGrid0 = Grid.getSubGridCellIdxs(sgi0)
                val subGrid1 = Grid.getSubGridCellIdxs(sgi1)
                val subGrid0Bools = subGrid0.map(idx => guesses.getOrElse(idx, Set.empty).contains(number))
                val subGrid1Bools = subGrid1.map(idx => guesses.getOrElse(idx, Set.empty).contains(number))

                val res = (getSubGridLine(subGrid0Bools), getSubGridLine(subGrid1Bools)) match {
                  case (Some(SubGridLine.Vertical(column)), _) => getGuessIdxByNumber(SubGrid.getCol(subGrid1)(column), guesses, number)
                  case (_, Some(SubGridLine.Vertical(column))) => getGuessIdxByNumber(SubGrid.getCol(subGrid0)(column), guesses, number)
                  case _ => None
                }
                if (res.isDefined) {
                  return res
                }
              }
          }
      }
      None
    }
  }

  private def getGuessIdxByNumber(idxs: Array[Int], guesses: Map[Int, Set[Int]], number: Int): Option[(Int, Int)] = {
    idxs.find(idx => guesses.getOrElse(idx, Set.empty).contains(number)).map(idx => (idx, number))
  }

  private def isSubGridLine(b0: Boolean, b1: Boolean, b2: Boolean, others: Set[Boolean]): Boolean = {
    // ttt
    (b0 && b1 && b2 && others.forall(_ == false)) ||
      // ftt
      (!b0 && b1 && b2 && others.forall(_ == false)) ||
      // tft
      (b0 && !b1 && b2 && others.forall(_ == false)) ||
      // ttf
      (b0 && b1 && !b2 && others.forall(_ == false))
  }

  private def getSubGridLine(subGridCells: Array[Boolean]): Option[SubGridLine] = {
    subGridCells match
      case Array(
      b0, b1, b2,
      b3, b4, b5,
      b6, b7, b8
      ) if isSubGridLine(b0, b1, b2, Set(b3, b4, b5, b6, b7, b8)) => Some(SubGridLine.Horizontal(0))
      case Array(
      b0, b1, b2,
      b3, b4, b5,
      b6, b7, b8
      ) if isSubGridLine(b3, b4, b5, Set(b0, b1, b2, b6, b7, b8)) => Some(SubGridLine.Horizontal(1))
      case Array(
      b0, b1, b2,
      b3, b4, b5,
      b6, b7, b8
      ) if isSubGridLine(b6, b7, b8, Set(b0, b1, b2, b3, b4, b5)) => Some(SubGridLine.Horizontal(2))
      case Array(
      b0, b1, b2,
      b3, b4, b5,
      b6, b7, b8
      ) if isSubGridLine(b0, b3, b6, Set(b1, b2, b4, b5, b7, b8)) => Some(SubGridLine.Vertical(0))
      case Array(
      b0, b1, b2,
      b3, b4, b5,
      b6, b7, b8
      ) if isSubGridLine(b1, b4, b7, Set(b0, b2, b3, b5, b6, b8)) => Some(SubGridLine.Vertical(1))
      case Array(
      b0, b1, b2,
      b3, b4, b5,
      b6, b7, b8
      ) if isSubGridLine(b2, b5, b8, Set(b0, b1, b3, b4, b6, b7)) => Some(SubGridLine.Vertical(2))
      case _ => None
  }
}
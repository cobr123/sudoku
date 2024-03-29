import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.time.{Seconds, Span}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps


class GridSpec extends AnyFunSpec with TimeLimitedTests {

  override val timeLimit: Span = Span(10, Seconds)

  it("solved Grid") {
    val c1 = SubGrid(5, 3, 4, 6, 7, 2, 1, 9, 8)
    val c2 = SubGrid(6, 7, 8, 1, 9, 5, 3, 4, 2)
    val c3 = SubGrid(9, 1, 2, 3, 4, 8, 5, 6, 7)
    val c4 = SubGrid(8, 5, 9, 4, 2, 6, 7, 1, 3)
    val c5 = SubGrid(7, 6, 1, 8, 5, 3, 9, 2, 4)
    val c6 = SubGrid(4, 2, 3, 7, 9, 1, 8, 5, 6)
    val c7 = SubGrid(9, 6, 1, 2, 8, 7, 3, 4, 5)
    val c8 = SubGrid(5, 3, 7, 4, 1, 9, 2, 8, 6)
    val c9 = SubGrid(2, 8, 4, 6, 3, 5, 1, 7, 9)
    val grid = Grid(c1, c2, c3, c4, c5, c6, c7, c8, c9)
    assert(Grid.isFinished(grid.cells))
  }

  it("solved Grid by rows") {
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

    assert(Grid.getRow(grid.cells)(0).mkString("", " ", "") === r1.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(1).mkString("", " ", "") === r2.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(2).mkString("", " ", "") === r3.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(3).mkString("", " ", "") === r4.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(4).mkString("", " ", "") === r5.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(5).mkString("", " ", "") === r6.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(6).mkString("", " ", "") === r7.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(7).mkString("", " ", "") === r8.nums.mkString("", " ", ""))
    assert(Grid.getRow(grid.cells)(8).mkString("", " ", "") === r9.nums.mkString("", " ", ""))

    assert(Grid.getCol(grid.cells)(0).mkString("", " ", "") === "5 6 1 8 4 7 9 2 3")
    assert(Grid.getCol(grid.cells)(1).mkString("", " ", "") === "3 7 9 5 2 1 6 8 4")
    assert(Grid.getCol(grid.cells)(2).mkString("", " ", "") === "4 2 8 9 6 3 1 7 5")
    assert(Grid.getCol(grid.cells)(3).mkString("", " ", "") === "6 1 3 7 8 9 5 4 2")
    assert(Grid.getCol(grid.cells)(4).mkString("", " ", "") === "7 9 4 6 5 2 3 1 8")
    assert(Grid.getCol(grid.cells)(5).mkString("", " ", "") === "8 5 2 1 3 4 7 9 6")
    assert(Grid.getCol(grid.cells)(6).mkString("", " ", "") === "9 3 5 4 7 8 2 6 1")
    assert(Grid.getCol(grid.cells)(7).mkString("", " ", "") === "1 4 6 2 9 5 8 3 7")
    assert(Grid.getCol(grid.cells)(8).mkString("", " ", "") === "2 8 7 3 1 6 4 5 9")
  }

  it("not finished Grid") {
    val c1 = SubGrid(5, 3, 4, 6, 7, 2, 1, 9, 8)
    val c2 = SubGrid(6, 7, 8, 1, 9, 5, 3, 4, 2)
    val c3 = SubGrid(9, 1, 2, 3, 4, 8, 5, 6, 7)
    val c4 = SubGrid(8, 5, 9, 4, 2, 6, 7, 1, 3)
    val c5 = SubGrid(7, 6, 1, 8, 5, 3, 9, 2, 4)
    val c6 = SubGrid(4, 2, 3, 7, 9, 1, 8, 5, 6)
    val c7 = SubGrid(9, 6, 1, 2, 8, 7, 3, 4, 5)
    val c8 = SubGrid(5, 3, 7, 4, 1, 9, 2, 8, 6)
    val c9 = SubGrid(5, 3, 3, 4, 1, 9, 2, 8, 6)
    val grid = Grid(c1, c2, c3, c4, c5, c6, c7, c8, c9)
    assert(!Grid.isFinished(grid.cells))
  }

  it("not finished SubGrid") {
    val subGrid1 = SubGrid(0, 3, 4, 6, 7, 2, 1, 9, 8)
    assert(!SubGrid.isFilledCorrect(subGrid1))

    val subGrid2 = SubGrid(3, 3, 4, 6, 7, 2, 1, 9, 8)
    assert(!SubGrid.isFilledCorrect(subGrid2))
  }

  it("getSubGrid") {
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

    assert(Grid.getSubGrid(grid.cells)(0).mkString("", " ", "") === "5 3 4 6 7 2 1 9 8")
    assert(Grid.getSubGrid(grid.cells)(1).mkString("", " ", "") === "6 7 8 1 9 5 3 4 2")
    assert(Grid.getSubGrid(grid.cells)(2).mkString("", " ", "") === "9 1 2 3 4 8 5 6 7")

    assert(Grid.getSubGrid(grid.cells)(3).mkString("", " ", "") === "8 5 9 4 2 6 7 1 3")
    assert(Grid.getSubGrid(grid.cells)(4).mkString("", " ", "") === "7 6 1 8 5 3 9 2 4")
    assert(Grid.getSubGrid(grid.cells)(5).mkString("", " ", "") === "4 2 3 7 9 1 8 5 6")

    assert(Grid.getSubGrid(grid.cells)(6).mkString("", " ", "") === "9 6 1 2 8 7 3 4 5")
    assert(Grid.getSubGrid(grid.cells)(7).mkString("", " ", "") === "5 3 7 4 1 9 2 8 6")
    assert(Grid.getSubGrid(grid.cells)(8).mkString("", " ", "") === "2 8 4 6 3 5 1 7 9")

    assert(SubGrid.getRow(Grid.getSubGrid(grid.cells)(8))(0).mkString("", " ", "") === "2 8 4")
    assert(SubGrid.getRow(Grid.getSubGrid(grid.cells)(8))(1).mkString("", " ", "") === "6 3 5")
    assert(SubGrid.getRow(Grid.getSubGrid(grid.cells)(8))(2).mkString("", " ", "") === "1 7 9")

    assert(SubGrid.getCol(Grid.getSubGrid(grid.cells)(8))(0).mkString("", " ", "") === "2 6 1")
    assert(SubGrid.getCol(Grid.getSubGrid(grid.cells)(8))(1).mkString("", " ", "") === "8 3 7")
    assert(SubGrid.getCol(Grid.getSubGrid(grid.cells)(8))(2).mkString("", " ", "") === "4 5 9")
  }

  it("get finished random Grid") {
    val initGridNums = Grid().cells
    val grid = Grid.solve(initGridNums)
    Grid.printGrid(grid.cells)
    assert(Grid.isFinished(grid.cells))
  }

  it("get finished from partly empty Grid") {
    val r1 = Row(0, 3, 0, 6, 0, 8, 9, 1, 2)
    val r2 = Row(0, 0, 0, 1, 9, 5, 3, 0, 8)
    val r3 = Row(0, 0, 0, 3, 4, 0, 5, 6, 7)
    val r4 = Row(0, 5, 9, 0, 6, 1, 0, 2, 3)
    val r5 = Row(4, 2, 6, 8, 0, 3, 7, 9, 1)
    val r6 = Row(0, 1, 3, 9, 2, 0, 8, 5, 6)
    val r7 = Row(9, 6, 0, 5, 3, 7, 0, 8, 4)
    val r8 = Row(2, 8, 7, 0, 1, 9, 6, 0, 5)
    val r9 = Row(3, 0, 5, 2, 8, 6, 1, 7, 0)
    val initGrid = Grid(Array(r1, r2, r3, r4, r5, r6, r7, r8, r9))
    val grid = Grid.solve(initGrid.cells)
    Grid.printGrid(grid.cells)
    assert(Grid.isFinished(grid.cells))
    assert(Grid.getRow(grid.cells)(0).apply(1) == 3)
    assert(Grid.getRow(grid.cells)(1).apply(3) == 1)
  }

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

  it("change solved grid by complexity") {
    Complexity.values.foreach { complexity =>
      val grid = getSolvedGrid
      Complexity.changeSolvedGridByComplexity(grid, complexity.solvedCellCount)
      assert(grid.cells.count(_ != 0) == complexity.solvedCellCount)
      println(s"${complexity.toString}: ${complexity.solvedCellCount}")
      Grid.printGrid(grid.cells)
    }
  }

  it("getGuesses on empty grid") {
    val grid = Grid()
    val guesses = Grid.getGuesses(grid.cells, 0)
    assert(guesses === Grid.availableValues)
  }

  it("getGuesses on empty row") {
    val r1 = Row(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val r2 = Row(6, 7, 2, 1, 9, 5, 3, 4, 8)
    val r3 = Row(1, 9, 8, 3, 4, 2, 5, 6, 7)
    val r4 = Row(8, 5, 9, 7, 6, 1, 4, 2, 3)
    val r5 = Row(4, 2, 6, 8, 5, 3, 7, 9, 1)
    val r6 = Row(7, 1, 3, 9, 2, 4, 8, 5, 6)
    val r7 = Row(9, 6, 1, 5, 3, 7, 2, 8, 4)
    val r8 = Row(2, 8, 7, 4, 1, 9, 6, 3, 5)
    val r9 = Row(3, 4, 5, 2, 8, 6, 1, 7, 9)
    val grid = Grid(Array(r1, r2, r3, r4, r5, r6, r7, r8, r9))
    val guesses = Grid.getGuesses(grid.cells, 0)
    assert(guesses === Set(5))
  }

  it("getGuesses on empty col") {
    val r1 = Row(0, 3, 4, 6, 7, 8, 9, 1, 2)
    val r2 = Row(0, 7, 2, 1, 9, 5, 3, 4, 8)
    val r3 = Row(0, 9, 8, 3, 4, 2, 5, 6, 7)
    val r4 = Row(0, 5, 9, 7, 6, 1, 4, 2, 3)
    val r5 = Row(0, 2, 6, 8, 5, 3, 7, 9, 1)
    val r6 = Row(0, 1, 3, 9, 2, 4, 8, 5, 6)
    val r7 = Row(0, 6, 1, 5, 3, 7, 2, 8, 4)
    val r8 = Row(0, 8, 7, 4, 1, 9, 6, 3, 5)
    val r9 = Row(0, 4, 5, 2, 8, 6, 1, 7, 9)
    val grid = Grid(Array(r1, r2, r3, r4, r5, r6, r7, r8, r9))
    val guesses = Grid.getGuesses(grid.cells, 0)
    assert(guesses === Set(5))
  }

  it("placeNumber on empty grid") {
    val grid = Grid()
    val canPlace1 = Grid.placeNumber(grid.cells, 0, 1)
    assert(canPlace1 === true)
  }

  it("placeNumber on empty row") {
    val r1 = Row(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val r2 = Row(6, 7, 2, 1, 9, 5, 3, 4, 8)
    val r3 = Row(1, 9, 8, 3, 4, 2, 5, 6, 7)
    val r4 = Row(8, 5, 9, 7, 6, 1, 4, 2, 3)
    val r5 = Row(4, 2, 6, 8, 5, 3, 7, 9, 1)
    val r6 = Row(7, 1, 3, 9, 2, 4, 8, 5, 6)
    val r7 = Row(9, 6, 1, 5, 3, 7, 2, 8, 4)
    val r8 = Row(2, 8, 7, 4, 1, 9, 6, 3, 5)
    val r9 = Row(3, 4, 5, 2, 8, 6, 1, 7, 9)
    val grid = Grid(Array(r1, r2, r3, r4, r5, r6, r7, r8, r9))
    val canPlace1 = Grid.placeNumber(grid.cells, 0, 1)
    assert(canPlace1 === false)
    val canPlace5 = Grid.placeNumber(grid.cells, 0, 5)
    assert(canPlace5 === true)
  }

  it("placeNumber on empty col") {
    val r1 = Row(0, 3, 4, 6, 7, 8, 9, 1, 2)
    val r2 = Row(0, 7, 2, 1, 9, 5, 3, 4, 8)
    val r3 = Row(0, 9, 8, 3, 4, 2, 5, 6, 7)
    val r4 = Row(0, 5, 9, 7, 6, 1, 4, 2, 3)
    val r5 = Row(0, 2, 6, 8, 5, 3, 7, 9, 1)
    val r6 = Row(0, 1, 3, 9, 2, 4, 8, 5, 6)
    val r7 = Row(0, 6, 1, 5, 3, 7, 2, 8, 4)
    val r8 = Row(0, 8, 7, 4, 1, 9, 6, 3, 5)
    val r9 = Row(0, 4, 5, 2, 8, 6, 1, 7, 9)
    val grid = Grid(Array(r1, r2, r3, r4, r5, r6, r7, r8, r9))
    val canPlace1 = Grid.placeNumber(grid.cells, 0, 1)
    assert(canPlace1 === false)
    val canPlace5 = Grid.placeNumber(grid.cells, 0, 5)
    assert(canPlace5 === true)
  }

  it("getLastNumberGuessInSubGrid 1") {
    val guesses = mutable.HashMap[Int, Set[Int]]()
    guesses += (0 -> Set(1, 2, 3, 4))
    guesses += (1 -> Set(1, 2, 3))
    guesses += (2 -> Set(1, 2, 3))
    guesses += (10 -> Set(1, 2, 3))
    guesses += (11 -> Set(1, 2, 3))

    Grid.getLastNumberGuessInSubGrid(guesses.toMap) match {
      case Some((idx, guesses)) =>
        assert(idx === 0)
        assert(guesses === Set(4))
      case None => fail()
    }
  }

  it("getLastNumberGuessInSubGrid 2") {
    val guesses = mutable.HashMap[Int, Set[Int]]()
    guesses += (1 -> Set(1, 2, 5, 7, 8))
    guesses += (2 -> Set(1, 2, 4, 5, 8))
    guesses += (9 -> Set(1, 2, 5, 6, 8))
    guesses += (10 -> Set(1, 2, 4, 5, 6))
    guesses += (11 -> Set(1, 2, 4, 5, 6, 8))
    guesses += (18 -> Set(1, 2, 5, 8))
    guesses += (20 -> Set(1, 2, 4, 5, 8))

    Grid.getLastNumberGuessInSubGrid(guesses.toMap) match {
      case Some((idx, guesses)) =>
        assert(idx === 1)
        assert(guesses === Set(7))
      case None => fail()
    }
  }

  it("getLastNumberGuessInSubGrid 3") {
    val guesses = List(
      (60, Set.empty), (61, Set(2, 4, 5, 6, 7, 9)), (62, Set(2, 4, 5, 6, 9)),
      (69, Set(1, 4, 5, 6)), (70, Set(4, 5, 6, 7, 9)), (71, Set.empty),
      (78, Set(1, 4, 5, 6)), (79, Set(2, 4, 5, 6, 7, 9)), (80, Set(1, 2, 4, 5, 6, 9))
    ).toMap

    assert(Grid.getLastNumberGuessInSubGrid(guesses) === None)
  }

  it("auto fill") {
    val r1 = Row(0, 0, 0, 0, 8, 9, 0, 0, 0)
    val r2 = Row(0, 0, 9, 0, 0, 0, 0, 0, 0)
    val r3 = Row(0, 0, 0, 1, 0, 0, 7, 8, 0)
    val r4 = Row(0, 0, 0, 0, 0, 0, 2, 1, 0)
    val r5 = Row(0, 8, 0, 0, 0, 0, 9, 0, 7)
    val r6 = Row(0, 0, 7, 0, 0, 0, 0, 0, 0)
    val r7 = Row(0, 0, 1, 0, 0, 0, 8, 0, 0)
    val r8 = Row(0, 0, 8, 2, 0, 0, 0, 0, 3)
    val r9 = Row(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val grid = Grid(Array(r1, r2, r3, r4, r5, r6, r7, r8, r9))
    assert(!Grid.isFinished(grid.cells))
    assert(grid.cells.count(_ != 0) === Complexity.Extreme.solvedCellCount)

    val guesses = grid.cells.zipWithIndex.filter(_._1 == 0).map {
      case (_, idx) =>
        val newGuesses = Grid.getGuesses(grid.cells, idx)
        (idx, newGuesses)
    }.toMap
    assert(guesses.filter(_._2.nonEmpty).keys.size == grid.cells.length - Complexity.Extreme.solvedCellCount)

    autofill(grid.cells, guesses)

    println(guesses.toArray.map(kv => (kv._1, kv._2.toArray.sorted.mkString(" Set(", ", ", ")"))).sortBy(_._1).mkString(",\n"))
    assert(guesses.nonEmpty)
    assert(!Grid.isFinished(grid.cells))
  }

  @tailrec
  private def autofill(cells: Array[Int], guesses: Map[Int, Set[Int]]): Unit = {
    guesses.find(_._2.size == 1).orElse(Grid.getLastNumberGuessInSubGrid(guesses)) match {
      case Some((idx, numbers)) =>
        val canPlace = Grid.getCanPlace(cells, idx, numbers.head)
        cells(idx) = numbers.head
        Grid.printGrid(cells, idx)
        if (canPlace) {
          autofill(cells, guesses.drop(idx))
        }
      case None =>
    }
  }

  it("getGhostCrossedByLine horizontal subGrid 0 - subGrid 1") {
    val guesses = List(
      (0, Set(2, 6, 9)), (1, Set(1, 4, 5, 6, 7, 9)), (2, Set(1, 4, 5, 6, 9)),
      (9, Set(2, 4, 5, 6)), (10, Set(1, 5, 6, 7, 9)), (11, Set.empty),
      (18, Set(1, 4, 5, 6)), (19, Set(1, 4, 5, 6, 7, 9)), (20, Set(1, 3, 4, 5, 6, 9)),

      (3, Set(1, 6, 9)), (4, Set(1, 4, 5, 6, 7, 9)), (5, Set(1, 4, 5, 6, 9)),
      (12, Set(2, 4, 5, 6)), (13, Set(2, 5, 6, 7, 9)), (14, Set.empty),
      (21, Set(1, 4, 5, 6)), (22, Set(1, 4, 5, 6, 7, 9)), (23, Set(1, 3, 4, 5, 6, 9))
    ).toMap

    assert(Grid.getGhostCrossedByLine(guesses) === Some((9, 2)))
  }

  it("getGhostCrossedByLine vertical subGrid 0 - subGrid 3") {
    val guesses = List(
      (0, Set(2, 6, 9)), (1, Set(1, 4, 5, 6, 7, 9)), (2, Set(1, 4, 5, 6, 9)),
      (9, Set(2, 4, 5, 6)), (10, Set(1, 5, 6, 7, 9)), (11, Set.empty),
      (18, Set(1, 4, 5, 6)), (19, Set(1, 4, 5, 6, 7, 9)), (20, Set(1, 3, 4, 5, 6, 9)),

      (27, Set(1, 6, 9)), (28, Set(1, 4, 5, 6, 7, 9)), (29, Set(1, 4, 5, 6, 9)),
      (36, Set(2, 4, 5, 6)), (37, Set(2, 5, 6, 7, 9)), (38, Set.empty),
      (45, Set(1, 4, 5, 6)), (46, Set(1, 4, 5, 6, 7, 9)), (47, Set(1, 3, 4, 5, 6, 9))
    ).toMap

    assert(Grid.getGhostCrossedByLine(guesses) === Some((36, 2)))
  }
}
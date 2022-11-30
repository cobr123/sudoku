import org.scalatest.funspec.AnyFunSpec


class GridSpec extends AnyFunSpec {

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
    assert(Grid.isFinished(grid))
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

    assert(Grid.isFinished(grid))

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
    assert(!Grid.isFinished(grid))
  }

  it("not finished SubGrid") {
    val subGrid1 = SubGrid(0, 3, 4, 6, 7, 2, 1, 9, 8)
    assert(!SubGrid.isFilledCorrect(subGrid1))

    val subGrid2 = SubGrid(3, 3, 4, 6, 7, 2, 1, 9, 8)
    assert(!SubGrid.isFilledCorrect(subGrid2))
  }

  it("get finished random Grid") {
    val initGridNums = Array.fill(9 * 9)(0)
    val grid = Grid.solve(initGridNums)
    Grid.printGrid(grid.cells)
    assert(Grid.isFinished(grid))
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
    assert(Grid.isFinished(grid))
    assert(Grid.getRow(grid.cells)(0).apply(1) == 3)
    assert(Grid.getRow(grid.cells)(1).apply(3) == 1)
  }
}
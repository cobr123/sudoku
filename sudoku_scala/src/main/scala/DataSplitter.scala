

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import scala.collection.mutable.ListBuffer
import scala.util.Random

object DataSplitter {

  val dir = "./data_set"

  def main(args: Array[String]): Unit = {
    val list: List[String] = Random.shuffle(new File(DataGen.dir).list.toList)
    val grids: List[Grid] = timed("reading grids") {
      list.map { fileName =>
        val fileContent = Files.readString(Paths.get(s"${DataGen.dir}/$fileName"), StandardCharsets.UTF_8)
        readFromString(fileContent)
      }
    }
    saveAugmentedCsv(grids, "all")
  }

  def timed[T](name: String)(f: => T): T = {
    print(name)
    val timeStart = System.currentTimeMillis()
    val result = f
    val timeEnd = System.currentTimeMillis()
    println(s", ${(timeEnd - timeStart) / 1000} sec")
    result
  }

  def saveAugmentedCsv(grids: List[Grid], csvFileName: String): Unit = {
    for (idx <- 0 to 80) {
      val csvPath = Paths.get(s"$dir/${csvFileName}_$idx.csv")
      timed(s"writing $csvPath") {
        val buffer = ListBuffer[String]()
        val headers = s"${
          (0 until 9 * 9).map(idx => s"cell_$idx")
            .mkString(",")
        }"
        buffer.append(headers)

        grids.foreach { grid =>
          val content = s"${grid.cells.mkString(",")}"
          buffer.append(content)

          for (solvedCellCount <- Complexity.Extreme.solvedCellCount until grid.cells.length by 8) {
            val augmentedGrid = Complexity.changeSolvedGridByComplexity(Grid(grid.cells.clone()), solvedCellCount)
            // recover main cell
            augmentedGrid.cells(idx) = grid.cells(idx)
            val content = s"${augmentedGrid.cells.mkString(",")}"
            buffer.append(content)
          }
        }

        val options = if (Files.exists(csvPath)) StandardOpenOption.TRUNCATE_EXISTING
        else StandardOpenOption.CREATE
        Files.write(csvPath, buffer.mkString("\n").getBytes(StandardCharsets.UTF_8), options)
      }
    }
  }

  def saveCsv(fileNames: List[String], csvFileName: String): Unit = {
    val csvPath = Paths.get(s"$dir/$csvFileName.csv")
    println(s"writing $csvPath")
    val headers = s"${
      (0 until 9 * 9).map(idx => s"cell_$idx")
        .mkString(",")
    }\n"

    val options = if (Files.exists(csvPath)) StandardOpenOption.TRUNCATE_EXISTING
    else StandardOpenOption.CREATE
    Files.write(csvPath, headers.getBytes(StandardCharsets.UTF_8), options)

    fileNames.foreach { fileName =>
      val fileContent = Files.readString(Paths.get(s"${DataGen.dir}/$fileName"), StandardCharsets.UTF_8)
      val grid: Grid = readFromString(fileContent)
      val content = s"${grid.cells.mkString(",")}\n"
      Files.write(csvPath, content.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    }
  }
}

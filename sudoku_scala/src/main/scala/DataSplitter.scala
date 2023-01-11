

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import scala.util.Random

object DataSplitter {

  val dir = "./data_set"

  def main(args: Array[String]): Unit = {
    val list: List[String] = Random.shuffle(new File(DataGen.dir).list.toList)
    saveAugmentedCsv(list, "all")
  }

  def saveAugmentedCsv(fileNames: List[String], csvFileName: String): Unit = {
    for (idx <- 0 to 80) {
      val csvPath = Paths.get(s"$dir/${csvFileName}_$idx.csv")
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

        for (solvedCellCount <- Complexity.Extreme.solvedCellCount until grid.cells.length by 8) {
          val augmentedGrid = Complexity.changeSolvedGridByComplexity(Grid(grid.cells.clone()), solvedCellCount)
          // recover main cell
          augmentedGrid.cells(idx) = grid.cells(idx)
          val content = s"${augmentedGrid.cells.mkString(",")}\n"
          Files.write(csvPath, content.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
        }
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

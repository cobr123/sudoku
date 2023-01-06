

import com.github.plokhotnyuk.jsoniter_scala.core.readFromString

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, OpenOption, Paths, StandardOpenOption}
import scala.util.Random

object DataSplitter {

  val dir = "./data_set"

  def main(args: Array[String]): Unit = {
    val list: List[String] = Random.shuffle(new File(DataGen.dir).list.toList)
    val (train, others) = list.splitAt(list.size / 2)
    val (test, valid) = others.splitAt(others.size / 2)
    saveCsv(train, "train")
    saveCsv(test, "test")
    saveCsv(valid, "valid")
    saveCsv(list, "all")
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
